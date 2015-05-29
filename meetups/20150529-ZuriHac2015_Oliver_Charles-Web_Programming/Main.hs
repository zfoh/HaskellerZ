{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad.Trans.Maybe
import Data.List (intersperse)
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Lucid
import Network.HTTP.Types.Status
import Web.Spock.Simple
import qualified Data.Vector as Vector
import qualified Database.PostgreSQL.Simple as Pg
import qualified Database.PostgreSQL.Simple.FromField as Pg
import qualified Database.PostgreSQL.Simple.FromRow as Pg
import qualified Database.PostgreSQL.Simple.ToRow as Pg

-- This SpockAction is parameterized to work with /any/ database,
-- session and application state.
helloSpock :: SpockAction database session state ()
helloSpock = do lucid helloSpockHtml

app :: SpockM Pg.Connection session state ()
app =
  do get "/" getProjects
     post "/projects" postProject
     get "/add-project" addProjectForm

main :: IO ()
main =
  do runSpock
       8000
       (spock sessionConfig dbConn initialState app)

sessionConfig :: SessionCfg ()
sessionConfig =
  SessionCfg "zurihac" (60 * 60) 0 True () Nothing

dbConn :: PoolOrConn Pg.Connection
dbConn =
  PCConn (ConnBuilder
            (Pg.connect
               Pg.defaultConnectInfo {Pg.connectUser = "zurihac"
                                     ,Pg.connectDatabase = "zurihac"})
            Pg.close
            (PoolCfg 5 5 60))

initialState :: ()
initialState = ()

pageTemplate :: Html () -> Html ()
pageTemplate contents =
  do html_ (do head_ (title_ "Hello!")
               body_ contents)

link :: Text -> Html () -> Html ()
link url caption = a_ [href_ url] caption

helloSpockHtml :: Html ()
helloSpockHtml =
  pageTemplate
    (do h1_ "Hello!"
        p_ "Hello, Lucid!"
        p_ (do "I love "
               link "http://haskell.org" "Haskell!"))

lucid :: Html () -> SpockAction database session state ()
lucid document = html (toStrict (renderText document))

data Project =
  Project {projectName :: Text
          ,projectDescription :: Text
          ,projectAuthors :: [Text]}

instance Pg.FromRow Project where
  fromRow = do
    name <- Pg.field
    description <- Pg.field
    authors <- fmap Vector.toList Pg.field
    return (Project name description authors)

sqlListAllProjects :: Pg.Query
sqlListAllProjects =
  [sql| SELECT name, description, authors
        FROM projects
        ORDER BY name |]

fetchAllProjects :: Pg.Connection -> IO [Project]
fetchAllProjects dbConn = Pg.query_ dbConn sqlListAllProjects

projectToRow :: Project -> Html ()
projectToRow project =
  tr_ (do td_ (toHtml (projectName project))
          td_ (toHtml (projectDescription project))
          td_ (commaSeparate (map toHtml (projectAuthors project))))
  where
    commaSeparate :: [Html ()] -> Html ()
    commaSeparate = mconcat . intersperse ", "

renderProjects :: [Project] -> Html ()
renderProjects projects =
  table_ (do thead_ (tr_ (do th_ "Name"
                             th_ "Description"
                             th_ "Authors"))
             tbody_ (foldMap projectToRow projects))

getProjects :: SpockAction Pg.Connection session state ()
getProjects =
  do allProjects <- runQuery fetchAllProjects
     lucid (pageTemplate
              (do h1_ "Projects"
                  renderProjects allProjects
                  link "/add-project" "Add Your Project!"))

projectFromPOST :: SpockAction database session state (Maybe Project)
projectFromPOST =
  runMaybeT
    (do name <-
          MaybeT (param "name")
        description <-
          MaybeT (param "description")
        authors <-
          sequence
            (map (\i -> MaybeT (param (pack ("author-" ++ show i))))
                 [0 .. 5])
        return (Project name description authors))

sqlAddProject :: Pg.Query
sqlAddProject =
  [sql| INSERT INTO projects (name, description, authors)
        VALUES (?, ?, ?) |]

instance Pg.ToRow Project where
  toRow (Project name description authors) =
    Pg.toRow (name, description, Vector.fromList authors)

insertProject :: Project -> Pg.Connection -> IO ()
insertProject project dbConn =
  do Pg.execute dbConn sqlAddProject project
     return ()

postProject :: SpockAction Pg.Connection session state ()
postProject =
  do maybeProject <- projectFromPOST
     case maybeProject of
       Nothing ->
         do lucid (p_ "Invalid submission")
            setStatus badRequest400
       Just project ->
         do runQuery (insertProject project)
            redirect "/"

addProjectForm :: SpockAction database session state ()
addProjectForm =
  do lucid
       (pageTemplate
         (do form_
               [method_ "post",action_ "/projects"]
               (do p_ (do label_ "Project"
                          input_ [name_ "name"])
                   p_ (do label_ "Description"
                          input_ [name_ "description"])
                   mapM_ authorRow [0 .. 5]
                   input_ [type_ "submit" ,value_ "Add Project"])))
  where authorRow i =
          do p_ (do label_ (toHtml ("Author #" ++ show i))
                    input_ [name_ (pack ("author-" ++ show i))])
