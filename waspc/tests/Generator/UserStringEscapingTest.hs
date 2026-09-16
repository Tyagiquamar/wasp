{-# LANGUAGE TupleSections #-}

module Generator.UserStringEscapingTest where

import Data.Aeson (object)
import Data.List (isPrefixOf, isSuffixOf, tails)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe, listToMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Fixtures (systemSPRoot)
import NeatInterpolation (trimming)
import StrongPath (relfile)
import qualified StrongPath as SP
import Test.Hspec
import qualified Util.Prisma as Util
import qualified Wasp.AppSpec as AS
import qualified Wasp.AppSpec.Api as AS.Api
import qualified Wasp.AppSpec.ApiNamespace as AS.ApiNamespace
import qualified Wasp.AppSpec.App as AS.App
import qualified Wasp.AppSpec.App.Auth as AS.Auth
import qualified Wasp.AppSpec.App.Auth.EmailVerification as AS.Auth.EmailVerification
import qualified Wasp.AppSpec.App.Auth.PasswordReset as AS.Auth.PasswordReset
import qualified Wasp.AppSpec.App.Client as AS.Client
import qualified Wasp.AppSpec.App.Db as AS.Db
import qualified Wasp.AppSpec.App.EmailSender as AS.EmailSender
import qualified Wasp.AppSpec.App.Wasp as AS.Wasp
import qualified Wasp.AppSpec.Core.Decl as AS.Decl
import qualified Wasp.AppSpec.Core.Ref as AS.Core.Ref
import qualified Wasp.AppSpec.Entity as AS.Entity
import qualified Wasp.AppSpec.ExtImport as AS.ExtImport
import qualified Wasp.AppSpec.Job as AS.Job
import qualified Wasp.AppSpec.Page as AS.Page
import qualified Wasp.AppSpec.Query as AS.Query
import qualified Wasp.AppSpec.Route as AS.Route
import qualified Wasp.ExternalConfig.Npm.PackageJson as Npm.PackageJson
import Wasp.Generator (genApp)
import Wasp.Generator.FileDraft (FileDraft (..))
import qualified Wasp.Generator.FileDraft.TemplateFileDraft as TmplFD
import Wasp.Generator.Monad (runGenerator)
import qualified Wasp.Generator.NpmWorkspaces as NW
import Wasp.Generator.Templates (compileAndRenderTemplate)
import qualified Wasp.Project.BuildType as BuildType
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx
import qualified Wasp.Version as WV

-- | Every free-form string a user can write in the Wasp config ends up in generated
-- JS/TS. This runs the whole generator on a spec where each of those strings contains
-- a double quote, renders the real templates, and checks that the quote is escaped
-- everywhere. It fails when a template quotes a raw value again (which leaves the
-- apostrophe unescaped, or HTML-escaped by Mustache) or when a generator stops
-- rendering a value through 'makeJsStringLiteral'.
spec_UserStringEscaping :: Spec
spec_UserStringEscaping = do
  renderedCodeFiles <- runIO renderGeneratedCodeFiles

  describe "user-provided strings in generated code" $ do
    it "never end up raw or HTML-escaped inside generated JS/TS" $ do
      -- Raw means a template interpolated the value without making a literal of it.
      -- HTML entities mean a template quoted the value and let Mustache HTML-escape it;
      -- generated code never legitimately contains an entity.
      [ dstPath
        | (dstPath, content) <- renderedCodeFiles,
          rawMarker `T.isInfixOf` content || any (`T.isInfixOf` content) htmlEntities
        ]
        `shouldBe` []

    it "are never wrapped in a second pair of quotes" $ do
      -- Catches a template that puts quotes around an already finished literal.
      [ dstPath
        | (dstPath, content) <- renderedCodeFiles,
          isAnyMarkerLiteralWrappedInQuotes content
        ]
        `shouldBe` []

    it "are escaped in every generated file that embeds them" $ do
      let filesWithEscapedMarker = [dstPath | (dstPath, content) <- renderedCodeFiles, escapedMarker `T.isInfixOf` content]
      [ expected
        | expected <- filesExpectedToEmbedUserStrings,
          not (any (expected `isSuffixOf`) filesWithEscapedMarker)
        ]
        `shouldBe` []
  where
    -- For each escaped marker, walks out to the double quotes delimiting its literal
    -- and checks the characters just outside them are not quotes as well.
    isAnyMarkerLiteralWrappedInQuotes :: Text -> Bool
    isAnyMarkerLiteralWrappedInQuotes content = any isLiteralWrapped markerPositions
      where
        strLength = T.length content
        charAt = T.index content

        markerPositions = [i | (i, rest) <- zip [0 ..] (tails (T.unpack content)), T.unpack escapedMarker `isPrefixOf` rest]

        isLiteralWrapped markerPos = case (maybeOpeningQuote, maybeClosingQuote) of
          (Just openingQuote, Just closingQuote) -> isQuoteAt (openingQuote - 1) || isQuoteAt (closingQuote + 1)
          -- Not inside a double-quoted literal at all.
          _ -> True
          where
            maybeOpeningQuote = listToMaybe [i | i <- [markerPos - 1, markerPos - 2 .. 0], isUnescapedDoubleQuoteAt i]
            maybeClosingQuote = listToMaybe [i | i <- [markerPos + T.length escapedMarker .. strLength - 1], isUnescapedDoubleQuoteAt i]

        isUnescapedDoubleQuoteAt i = charAt i == '"' && (i == 0 || charAt (i - 1) /= '\\')
        isQuoteAt i = i >= 0 && i < strLength && charAt i `elem` ['\'', '"']

    renderGeneratedCodeFiles :: IO [(FilePath, Text)]
    renderGeneratedCodeFiles = case runGenerator (genApp appSpecWithMarkers) of
      (_, Left errors) -> fail $ "Generating the app failed: " ++ show errors
      (_, Right fileDrafts) ->
        sequence
          [ (dstPath,) <$> compileAndRenderTemplate (TmplFD._srcPathInTmplDir draft) (fromMaybe (object []) (TmplFD._tmplData draft))
          | FileDraftTemplateFd draft <- fileDrafts,
            let dstPath = SP.fromRelFile (TmplFD._dstPath draft),
            isCodeFile dstPath
          ]

    isCodeFile dstPath = any (`isSuffixOf` dstPath) [".ts", ".tsx", ".js", ".jsx", ".mjs", ".cjs"]

    -- Generated files that embed at least one of the marked strings. Each of these
    -- had, or could have, a template that quoted the raw value.
    filesExpectedToEmbedUserStrings =
      [ "server/src/routes/apis/index.ts", -- api and apiNamespace paths
        "server/src/auth/providers/config/email.ts", -- fromField, email client routes
        "server/src/plugins/virtualUserModules.js", -- virtual module ids and import paths
        "sdk/wasp/client/router/index.ts", -- route paths
        "sdk/wasp/client/app/pages/createAuthRequiredPage.jsx", -- onAuthFailedRedirectTo
        "sdk/wasp/client/app/pages/OAuthCallback.tsx", -- onAuthSucceededRedirectTo
        "sdk/wasp/auth/forms/internal/common/LoginSignupForm.tsx", -- onAuthSucceededRedirectTo
        "sdk/wasp/server/auth/utils.ts", -- redirect paths on the server
        "sdk/wasp/server/email/core/helpers.ts", -- emailSender.defaultFrom
        "sdk/wasp/server/jobs/trickyJob.ts", -- cron
        "sdk/wasp/client/vite/plugins/waspConfig.ts", -- client.baseDir
        "sdk/wasp/client/vite/virtual-files/files/client-entry.tsx", -- client.baseDir
        "sdk/wasp/client/vite/plugins/virtualUserModules.ts", -- virtual module ids and import paths
        "sdk/wasp/wasp-user-virtual-modules.d.ts" -- virtual module ids
      ]

    rawMarker = T.pack marker
    escapedMarker = T.pack "tricky\\\"value"
    htmlEntities = map T.pack ["&#39;", "&quot;", "&amp;", "&lt;", "&gt;"]

-- | The substring planted in every free-form user string of the spec below.
marker :: String
marker = "tricky\"value"

appSpecWithMarkers :: AS.AppSpec
appSpecWithMarkers =
  AS.AppSpec
    { AS.decls =
        [ AS.Decl.makeDecl "TestApp" app,
          AS.Decl.makeDecl userEntityName userEntity,
          AS.Decl.makeDecl pageName page,
          AS.Decl.makeDecl routeName route,
          AS.Decl.makeDecl "trickyApi" api,
          AS.Decl.makeDecl "trickyNamespace" apiNamespace,
          AS.Decl.makeDecl "trickyJob" job,
          AS.Decl.makeDecl "trickyQuery" query
        ],
      AS.prismaSchema = prismaSchema,
      AS.waspProjectDir = systemSPRoot SP.</> [SP.reldir|test/|],
      AS.externalCodeFiles = [],
      AS.packageJson =
        Npm.PackageJson.PackageJson
          { Npm.PackageJson.name = "testApp",
            Npm.PackageJson.version = Nothing,
            Npm.PackageJson.dependencies = M.empty,
            Npm.PackageJson.devDependencies = M.empty,
            Npm.PackageJson.workspaces = Just $ S.toList NW.requiredWorkspaceGlobs,
            Npm.PackageJson.wasp = Nothing
          },
      AS.buildType = BuildType.Development,
      AS.migrationsDir = Nothing,
      AS.devEnvVarsClient = [],
      AS.devEnvVarsServer = [],
      AS.userDockerfileContents = Nothing,
      AS.devDatabaseUrl = Nothing,
      AS.srcTsConfigPath = [relfile|tsconfig.json|]
    }
  where
    app =
      AS.App.App
        { AS.App.wasp = AS.Wasp.Wasp {AS.Wasp.version = "^" ++ show WV.waspVersion},
          AS.App.title = "Test App",
          AS.App.deployment = Nothing,
          AS.App.db = Just AS.Db.Db {AS.Db.seeds = Nothing, AS.Db.prismaSetupFn = Nothing},
          AS.App.server = Nothing,
          AS.App.client =
            Just
              AS.Client.Client
                { AS.Client.setupFn = Nothing,
                  AS.Client.rootComponent = Nothing,
                  AS.Client.baseDir = Just ("/" ++ marker ++ "/"),
                  -- Loaded through a client-side virtual user module.
                  AS.Client.envValidationSchema = Just (markedExtImport "clientEnvSchema")
                },
          AS.App.auth = Just auth,
          AS.App.head = Nothing,
          AS.App.emailSender =
            Just
              AS.EmailSender.EmailSender
                { AS.EmailSender.provider = AS.EmailSender.SMTP,
                  AS.EmailSender.defaultFrom = Just markedFromField
                },
          AS.App.webSocket = Nothing
        }

    auth =
      AS.Auth.Auth
        { AS.Auth.userEntity = AS.Core.Ref.Ref userEntityName,
          AS.Auth.methods =
            AS.Auth.AuthMethods
              { AS.Auth.usernameAndPassword = Nothing,
                AS.Auth.slack = Nothing,
                AS.Auth.discord = Nothing,
                AS.Auth.google = Just AS.Auth.ExternalAuthConfig {AS.Auth.configFn = Nothing, AS.Auth.userSignupFields = Nothing},
                AS.Auth.gitHub = Nothing,
                AS.Auth.keycloak = Nothing,
                AS.Auth.microsoft = Nothing,
                AS.Auth.email =
                  Just
                    AS.Auth.EmailAuthConfig
                      { AS.Auth.userSignupFields = Nothing,
                        AS.Auth.fromField = markedFromField,
                        AS.Auth.emailVerification =
                          AS.Auth.EmailVerification.EmailVerificationConfig
                            { AS.Auth.EmailVerification.getEmailContentFn = Nothing,
                              AS.Auth.EmailVerification.clientRoute = AS.Core.Ref.Ref routeName
                            },
                        AS.Auth.passwordReset =
                          AS.Auth.PasswordReset.PasswordResetConfig
                            { AS.Auth.PasswordReset.getEmailContentFn = Nothing,
                              AS.Auth.PasswordReset.clientRoute = AS.Core.Ref.Ref routeName
                            }
                      }
              },
          AS.Auth.onAuthFailedRedirectTo = "/" ++ marker ++ "/login",
          AS.Auth.onAuthSucceededRedirectTo = Just ("/" ++ marker ++ "/home"),
          AS.Auth.onBeforeSignup = Nothing,
          AS.Auth.onAfterSignup = Nothing,
          AS.Auth.onAfterEmailVerified = Nothing,
          AS.Auth.onBeforeOAuthRedirect = Nothing,
          AS.Auth.onBeforeLogin = Nothing,
          AS.Auth.onAfterLogin = Nothing
        }

    markedFromField =
      AS.EmailSender.EmailFromField
        { AS.EmailSender.name = Just marker,
          AS.EmailSender.email = marker ++ "@example.com"
        }

    userEntityName = "User"
    userEntity =
      AS.Entity.makeEntity $
        Psl.Model.Body $
          Psl.WithCtx.empty <$> [Psl.Model.ElementField $ makeIdField "id" Psl.Model.String]

    makeIdField name typ =
      Psl.Model.Field
        { Psl.Model._name = name,
          Psl.Model._type = typ,
          Psl.Model._typeModifiers = [],
          Psl.Model._attrs =
            [ Psl.Attribute.Attribute
                { Psl.Attribute._attrName = "id",
                  Psl.Attribute._attrArgs = []
                },
              Psl.Attribute.Attribute
                { Psl.Attribute._attrName = "default",
                  Psl.Attribute._attrArgs = [Psl.Argument.ArgUnnamed (Psl.Argument.FuncExpr "autoincrement" [])]
                }
            ]
        }

    prismaSchema =
      Util.getPrismaSchema
        [trimming|
          datasource db {
            provider = "postgresql"
            url      = env("DATABASE_URL")
          }
          generator client {
            provider = "prisma-client-js"
          }
          model User {
            id Int @id @default(autoincrement())
          }
        |]

    pageName = "TrickyPage"
    page = AS.Page.Page {AS.Page.component = markedExtImport "Page", AS.Page.authRequired = Just True}

    routeName = "TrickyRoute"
    route =
      AS.Route.Route
        { AS.Route.path = "/" ++ marker ++ "/:id",
          AS.Route.to = AS.Core.Ref.Ref pageName,
          AS.Route.lazy = Nothing,
          AS.Route.prerender = []
        }

    api =
      AS.Api.Api
        { AS.Api.fn = markedExtImport "apiFn",
          AS.Api.middlewareConfigFn = Nothing,
          AS.Api.entities = Nothing,
          AS.Api.httpRoute = (AS.Api.GET, "/api/" ++ marker),
          AS.Api.auth = Nothing
        }

    apiNamespace =
      AS.ApiNamespace.ApiNamespace
        { AS.ApiNamespace.middlewareConfigFn = markedExtImport "namespaceMiddleware",
          AS.ApiNamespace.path = "/" ++ marker
        }

    job =
      AS.Job.Job
        { AS.Job.executor = AS.Job.PgBoss,
          AS.Job.perform = AS.Job.Perform {AS.Job.fn = markedExtImport "jobFn", AS.Job.executorOptions = Nothing},
          AS.Job.schedule =
            Just
              AS.Job.Schedule
                { AS.Job.cron = "*/5 * * * " ++ marker,
                  AS.Job.args = Nothing,
                  AS.Job.executorOptions = Nothing
                },
          AS.Job.entities = Nothing
        }

    -- Operations are loaded through virtual user modules, whose ids are derived
    -- from the user's import path.
    query =
      AS.Query.Query
        { AS.Query.fn = markedExtImport "queryFn",
          AS.Query.entities = Nothing,
          AS.Query.auth = Nothing
        }

    -- An import from a user file whose directory name carries the marker.
    markedExtImport name =
      AS.ExtImport.ExtImport
        (AS.ExtImport.ExtImportField name)
        (fromJust $ SP.parseRelFileP (marker ++ "/module"))
        Nothing
