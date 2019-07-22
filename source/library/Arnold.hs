{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Arnold
  ( defaultMain
  , makeLeaderboard
  , Count(..)
  , Exercise(..)
  , Rank(..)
  , Score(..)
  , UserId(..)
  , Workout(..)
  , WorkoutId(..)
  )
where

import qualified Control.Exception as Exception
import qualified Control.Monad as Monad
import qualified Crypto.Hash as Crypto
import qualified Crypto.MAC.HMAC as Crypto
import qualified Data.ByteArray as Memory
import qualified Data.ByteArray.Encoding as Memory
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Csv as Csv
import qualified Data.Default as Default
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.String as String
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import qualified Data.Time as Time
import qualified Data.UUID as Uuid
import qualified Data.UUID.V4 as Uuid
import qualified Data.Vector as Vector
import qualified Data.Version as Version
import qualified Database.SQLite.Simple as Sql
import qualified Database.SQLite.Simple.FromField as Sql
import qualified Database.SQLite.Simple.QQ as Sql
import qualified Database.SQLite.Simple.ToField as Sql
import qualified Lucid
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Middleware.Gzip as Middleware
import qualified Network.Wai.Middleware.RequestLogger as Middleware
import qualified Numeric.Natural as Natural
import qualified Paths_arnold as Package
import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified Text.RawString.QQ as QQ
import qualified Text.Read as Read
import qualified Web.FormUrlEncoded as Form
import qualified Web.HttpApiData as Form
import qualified Web.Scotty as Scotty

defaultMain :: IO ()
defaultMain = do
  config <- getConfig
  Sql.withConnection (configDatabaseFile config) $ \connection -> do
    mapM_ (Sql.execute_ connection) migrations
    Scotty.scottyOpts (configToOptions config) $ application config connection

application :: Config -> Sql.Connection -> Scotty.ScottyM ()
application config connection = do
  Scotty.defaultHandler defaultAction

  Scotty.middleware Middleware.logStdout
  Scotty.middleware $ Middleware.gzip Middleware.def

  Scotty.get "/" $ getRootAction connection
  Scotty.get "/favicon.ico" getFaviconAction
  Scotty.get "/ping" getPingAction
  Scotty.post "/slack" $ postSlackAction config connection
  Scotty.get "/workouts.csv" $ getWorkoutsAction connection

  Scotty.notFound notFoundAction

defaultAction :: LazyText.Text -> Scotty.ActionM ()
defaultAction problem = do
  Scotty.liftAndCatchIO $ LazyText.hPutStrLn IO.stderr problem
  Scotty.text "500 Internal Server Error"

getRootAction :: Sql.Connection -> Scotty.ActionM ()
getRootAction connection = do
  -- Note that this would have to change if this service ever ran on a server
  -- that wasn't in the same time zone as the office. Doing this was easy and
  -- preferrable to figuring out if the current zone is EDT or EST.
  now <- Scotty.liftAndCatchIO Time.getZonedTime
  let
    today = Time.localDay $ Time.zonedTimeToLocalTime now
    zone = Time.zonedTimeZone now

  workouts <- Scotty.liftAndCatchIO $ selectWorkouts connection zone
  users <- Scotty.liftAndCatchIO $ selectUsers connection
  let
    leaderboard = makeLeaderboard workouts
    names = makeUserIdsToNames users
    personalBests = getPersonalBests workouts
    dailyBests = getDailyBests workouts
    dailyWorkouts = filter ((== today) . workoutDay) workouts
    totals = getTotals workouts

  Scotty.html . Lucid.renderText . Lucid.doctypehtml_ $ do

    Lucid.head_ $ do
      Lucid.meta_ [Lucid.charset_ "utf-8"]
      Lucid.meta_
        [ Lucid.name_ "viewport"
        , Lucid.content_ "initial-scale = 1, width = device-width"
        ]
      Lucid.title_ "Arnold"
      Lucid.style_ [QQ.r|
        body {
          font: 1em/1.5em sans-serif;
          margin: 0 auto;
          max-width: 40em;
        }
        .centered {
          text-align: center;
        }
        .table {
          width: 100%;
        }
        .table th {
          border-bottom: thin solid;
        }
        .table tr:nth-child(even) {
          background: #eee;
        }
        .table tr:hover {
          background: #ffe;
        }
      |]

    Lucid.body_ $ do
      Lucid.div_ [Lucid.class_ "centered"] $ Lucid.h1_ "Arnold"

      Lucid.h2_ [Lucid.class_ "centered"] "Leaderboard"
      Lucid.table_ [Lucid.class_ "table"] $ do
        Lucid.thead_ . Lucid.tr_ $ do
          Lucid.th_ "Rank"
          Lucid.th_ "Person"
          Lucid.th_ "Score"
        Lucid.tbody_ . Monad.forM_ leaderboard $ \(rank, score, userId) ->
          Lucid.tr_ $ do
            Lucid.td_ $ Lucid.toHtml rank
            Lucid.td_ . Lucid.toHtml $ getUserName names userId
            Lucid.td_ $ Lucid.toHtml score

      Lucid.h2_ [Lucid.class_ "centered"] "Totals"
      Lucid.table_ [Lucid.class_ "table"] $ do
        Lucid.thead_ . Lucid.tr_ $ do
          Lucid.th_ "Exercise"
          Lucid.th_ "Count"
        Lucid.tbody_ . Monad.forM_ totals $ \(exercise, count) ->
          Lucid.tr_ $ do
            Lucid.td_ $ Lucid.toHtml exercise
            Lucid.td_ $ Lucid.toHtml count

      Lucid.h2_ [Lucid.class_ "centered"] "Daily bests"
      Lucid.details_ $ do
        Lucid.summary_ "Click to expand/collapse daily bests."
        Lucid.table_ [Lucid.class_ "table"] $ do
          Lucid.thead_ . Lucid.tr_ $ do
            Lucid.th_ "Day"
            Lucid.th_ "Exercise"
            Lucid.th_ "Person"
            Lucid.th_ "Count"
          Lucid.tbody_
            . Monad.forM_ dailyBests
            $ \(day, exercise, userId, count) -> Lucid.tr_ $ do
                Lucid.td_ . Lucid.toHtml $ formatTime "%Y-%m-%d" day
                Lucid.td_ $ Lucid.toHtml exercise
                Lucid.td_ . Lucid.toHtml $ getUserName names userId
                Lucid.td_ $ Lucid.toHtml count

      Lucid.h2_ [Lucid.class_ "centered"] "Personal Bests"
      Monad.forM_ [ExercisePushUp, ExerciseSquat, ExercisePlank]
        $ \exercise -> Lucid.details_ $ do
            Lucid.summary_ $ do
              "Click to expand/collapse "
              Lucid.toHtml exercise
              "s."
            Lucid.table_ [Lucid.class_ "table"] $ do
              Lucid.thead_ . Lucid.tr_ $ do
                Lucid.th_ "Person"
                Lucid.th_ "Count"
              Lucid.tbody_
                . Monad.forM_ (Map.findWithDefault [] exercise personalBests)
                $ \(userId, count) -> Lucid.tr_ $ do
                    Lucid.td_ . Lucid.toHtml $ getUserName names userId
                    Lucid.td_ $ Lucid.toHtml count

      Lucid.h2_ [Lucid.class_ "centered"] "Today's workouts"
      Lucid.details_ $ do
        Lucid.summary_ "Click to expand/collapse today's workouts."
        Lucid.p_ [Lucid.class_ "centered"] $ do
          "Download all workouts as "
          Lucid.a_ [Lucid.href_ "/workouts.csv"] "a CSV"
          "."
        Lucid.table_ [Lucid.class_ "table"] $ do
          Lucid.thead_ . Lucid.tr_ $ do
            Lucid.th_ "Date"
            Lucid.th_ "Time"
            Lucid.th_ "Exercise"
            Lucid.th_ "Person"
            Lucid.th_ "Count"
          Lucid.tbody_ . Monad.forM_ dailyWorkouts $ \workout -> Lucid.tr_ $ do
            let time = workoutRecordedAt workout
            Lucid.td_ . Lucid.toHtml $ formatTime "%Y-%m-%d" time
            Lucid.td_ . Lucid.toHtml $ formatTime "%-I:%M %p ET" time
            Lucid.td_ . Lucid.toHtml $ workoutExercise workout
            Lucid.td_ . Lucid.toHtml . getUserName names $ workoutUserId
              workout
            Lucid.td_ . Lucid.toHtml $ workoutCount workout

      Lucid.div_ [Lucid.class_ "centered"] $ Lucid.p_ "\x1F4AA"

getTotals :: [Workout Time.ZonedTime] -> [(Exercise, Count)]
getTotals = Map.toList . fmap (foldMap workoutCount) . groupBy workoutExercise

getDailyBests
  :: [Workout Time.ZonedTime] -> [(Time.Day, Exercise, UserId, Count)]
getDailyBests =
  concatMap (\(d, xs) -> fmap (\(e, (u, c)) -> (d, e, u, c)) xs)
    . Map.toDescList
    . fmap
        (Maybe.mapMaybe (\(k, mv) -> (,) <$> Just k <*> mv)
        . Map.toList
        . fmap
            (Maybe.listToMaybe
            . List.sortOn (Ord.Down . snd)
            . Map.toList
            . fmap (foldMap workoutCount)
            . groupBy workoutUserId
            )
        . groupBy workoutExercise
        )
    . groupBy workoutDay

getPersonalBests
  :: [Workout Time.ZonedTime] -> Map.Map Exercise [(UserId, Count)]
getPersonalBests =
  fmap (fmap snd)
    . groupBy fst
    . List.sortOn (Ord.Down . snd . snd)
    . concatMap (\(u, m) -> fmap (\(e, c) -> (e, (u, c))) $ Map.toList m)
    . Map.toList
    . fmap
        (Map.unionsWith max
        . fmap (fmap (foldMap workoutCount) . groupBy workoutExercise)
        . groupBy workoutDay
        )
    . groupBy workoutUserId

-- | Given a bunch of workouts, calculates each person's rank (and score). This
-- is a little more complicated than you might think in order to handle ties.
makeLeaderboard :: [Workout Time.ZonedTime] -> [(Rank, Score, UserId)]
makeLeaderboard =
  fmap (\(r, (s, i)) -> (r, s, i))
    . rankBy (Ord.Down . fst)
    . concatMap (\(s, is) -> fmap (\i -> (s, i)) is)
    . Map.toDescList
    . fmap (fmap fst) -- Remove score from values since it's the key.
    . groupBy snd -- Group tuples by score.
    . Map.toList
    . makeScoreboard

-- | Given a bunch of workouts, calculates each person's score. This function
-- is provided simply for convenience around 'scoreParticipation',
-- 'scoreImprovement', and 'scoreBest'.
makeScoreboard :: [Workout Time.ZonedTime] -> Map.Map UserId Score
makeScoreboard workouts = Map.unionsWith
  (<>)
  [scoreParticipation workouts, scoreImprovement workouts, scoreBest workouts]

-- | Each person gets one point for every one of their workouts. Each person
-- can only get up to eight points per day here.
scoreParticipation :: [Workout Time.ZonedTime] -> Map.Map UserId Score
scoreParticipation =
  fmap
      (Foldable.fold
      . fmap (min (Score 8) . foldMap (const $ Score 1))
      . groupBy workoutDay
      )
    . groupBy workoutUserId

-- | This is the most complicated part of the scoring. Each person gets four
-- points every time that they beat their previous daily best.
scoreImprovement :: [Workout Time.ZonedTime] -> Map.Map UserId Score
scoreImprovement = fmap scorePersonalImprovement . groupBy workoutUserId

-- | Since all of the 'scoreImprovement' logic is actually per-person, this
-- helper function takes a list of workouts from a single user and does all the
-- interesting work.
scorePersonalImprovement :: [Workout Time.ZonedTime] -> Score
scorePersonalImprovement =
  fst
    . foldl scorePersonalImprovementHelper (Score 0, Map.empty)
    . concatMap snd
    . Map.toAscList
    . fmap (Map.toList . fmap (foldMap workoutCount) . groupBy workoutExercise)
    . groupBy workoutDay

workoutDay :: Workout Time.ZonedTime -> Time.Day
workoutDay = Time.localDay . Time.zonedTimeToLocalTime . workoutRecordedAt

-- | This mostly exists to make 'scorePersonalImprovement' look nicer
-- syntactically. That being said it also captures some complicated logic on
-- its own: Figuring out if a person beat their personal best and carrying
-- along a map of the personal bests for each exercise.
scorePersonalImprovementHelper
  :: (Score, Map.Map Exercise Count)
  -> (Exercise, Count)
  -> (Score, Map.Map Exercise Count)
scorePersonalImprovementHelper (score, bests) (exercise, count) =
  ( case Map.lookup exercise bests of
    -- If this is their first time doing the exercise, they don't get the bonus
    -- points for being their (nonexistent) personal best.
    Nothing -> score
    Just previous -> if previous >= count
      -- Similarly if their previous score is better then they don't get the
      -- bonus points.
      then score
      -- However if they did beat their previous personal best then they do get
      -- the bonus points.
      else score <> Score 4
  , Map.insertWith max exercise count bests
  )

-- | The person who does the most workouts on each day gets two points. If
-- multiple people tied for first place, they all get points.
scoreBest :: [Workout Time.ZonedTime] -> Map.Map UserId Score
scoreBest =
  Map.fromListWith (<>)
    . fmap (\userId -> (userId, Score 2))
    . concat
    . Map.elems
    . fmap getBestOfDay
    . groupBy workoutDay

-- | Given a list of workouts taken from the same day, returns the people who
-- did the most exercises on that day. Usually there will only be one person.
-- However if there's a tie because many people did the same number of
-- exercises then all those people will be returned.
getBestOfDay :: [Workout time] -> [UserId]
getBestOfDay =
  maybe [] snd
    . Map.lookupMax -- Find the users with the highest exercise count.
    . fmap (fmap fst) -- Remove count from values since it's the key.
    . groupBy snd -- Group by the total exercise count for the day.
    . Map.toList
    . fmap (foldMap workoutCount)
    . groupBy workoutUserId

selectWorkouts
  :: Sql.Connection -> Time.TimeZone -> IO [Workout Time.ZonedTime]
selectWorkouts connection zone = do
  workouts <- Sql.query_
    connection
    [Sql.sql| select * from workouts order by recorded_at desc, user_id asc |]
  pure $ fmap (inTimeZone zone) workouts

inTimeZone :: Time.TimeZone -> Workout Time.UTCTime -> Workout Time.ZonedTime
inTimeZone zone workout = workout
  { workoutRecordedAt = Time.utcToZonedTime zone $ workoutRecordedAt workout
  }

selectUsers :: Sql.Connection -> IO [User]
selectUsers connection =
  Sql.query_ connection [Sql.sql| select * from users order by id asc |]

makeUserIdsToNames :: [User] -> Map.Map UserId Name
makeUserIdsToNames =
  Map.fromList . fmap (\user -> (userId user, userName user))

getUserName :: Map.Map UserId Name -> UserId -> Text.Text
getUserName names userId_ = case Map.lookup userId_ names of
  Nothing -> unwrapUserId userId_
  Just name -> unwrapName name

getPingAction :: Scotty.ActionM ()
getPingAction = Scotty.text "pong"

getFaviconAction :: Scotty.ActionM ()
getFaviconAction = do
  Scotty.setHeader "Content-Type" "image/x-icon"
  favicon <- Scotty.liftAndCatchIO $ Package.getDataFileName "favicon.ico"
  Scotty.file favicon

postSlackAction :: Config -> Sql.Connection -> Scotty.ActionM ()
postSlackAction config connection = do
  body <- Scotty.body
  checkSlackSignature (configSlackSigningSecret config) body
  payload <- getPayload body

  Scotty.liftAndCatchIO . upsertUser connection $ payloadToUser payload
  uuid <- Scotty.liftAndCatchIO Uuid.nextRandom
  now <- Scotty.liftAndCatchIO Time.getCurrentTime
  workout <- case payloadToWorkout uuid now payload of
    Left problem ->
      bail
        $ "I could not understand what you said ("
        <> problem
        <> "). I expect commands to match `/arnold [count] [workout]`, like "
        <> "`/arnold 10 squats` or `/arnold 1 push up`."
    Right workout -> pure workout
  Scotty.liftAndCatchIO $ insertWorkout connection workout

  Scotty.text
    . LazyText.pack
    $ "Great job doing "
    <> pluralize
         (exerciseToString $ workoutExercise workout)
         (unwrapCount $ workoutCount workout)
    <> "!"

payloadToUser :: Payload -> User
payloadToUser payload =
  User { userId = payloadUserId payload, userName = payloadUserName payload }

pluralize :: (Eq a, Num a, Show a) => String -> a -> String
pluralize word count =
  show count <> " " <> word <> if count == 1 then "" else "s"

insertWorkout :: Sql.Connection -> Workout Time.UTCTime -> IO ()
insertWorkout connection = Sql.execute
  connection
  [Sql.sql|
    insert into workouts ( id, recorded_at, exercise, user_id, count )
    values ( ?, ?, ?, ?, ? )
  |]

payloadToWorkout
  :: Uuid.UUID -> time -> Payload -> Either String (Workout time)
payloadToWorkout uuid time payload = do
  let command = payloadCommand payload
  Monad.when (command /= "/arnold")
    . Left
    $ "invalid command: "
    <> show command
  let text = payloadText payload
  (count, exercise) <- case Text.words text of
    rawCount : rawExercises -> do
      count <- case Read.readMaybe $ Text.unpack rawCount of
        Nothing -> Left $ "invalid Count: " <> show rawCount
        Just count -> Right $ Count count
      let rawExercise = Text.unpack $ Text.unwords rawExercises
      exercise <- case parseExercise rawExercise of
        Nothing -> Left $ "invalid Exercise: " <> show rawExercise
        Just exercise -> Right exercise
      Right (count, exercise)
    _ -> Left $ "invalid text: " <> show text
  Right Workout
    { workoutId = WorkoutId uuid
    , workoutRecordedAt = time
    , workoutExercise = exercise
    , workoutUserId = payloadUserId payload
    , workoutCount = count
    }

upsertUser :: Sql.Connection -> User -> IO ()
upsertUser connection user = Sql.withTransaction connection $ do
  maybeUser <- selectUser connection $ userId user
  case maybeUser of
    Nothing -> insertUser connection user
    Just _ -> updateUser connection user

selectUser :: Sql.Connection -> UserId -> IO (Maybe User)
selectUser connection userId_ = do
  users <- Sql.query connection [Sql.sql| select * from users where id = ? |]
    $ Sql.Only userId_
  pure $ Maybe.listToMaybe users

updateUser :: Sql.Connection -> User -> IO ()
updateUser connection user = Sql.execute
  connection
  [Sql.sql| update users set name = ? where id = ? |]
  (userName user, userId user)

insertUser :: Sql.Connection -> User -> IO ()
insertUser connection = Sql.execute
  connection
  [Sql.sql| insert into users ( id, name ) values ( ?, ? ) |]

getPayload :: LazyByteString.ByteString -> Scotty.ActionM Payload
getPayload form = case Form.urlDecodeAsForm form of
  Left problem ->
    bail
      $ "I could not understand the request from Slack because the HTTP body "
      <> "was not the form that I expected ("
      <> Text.unpack problem
      <> ")."
  Right payload -> pure payload

-- | This is similar to 'fail' in that it takes a message and immediately sends
-- a response. That's where the similarities end though. Instead of returning
-- an HTTP 500 this will return an HTTP 200. That probably surprises you, but
-- it's what Slack wants! <https://api.slack.com/slash-commands>
--
-- > It would be tempting [...] to return an HTTP 500 response to the initial
-- > command, but this isn't the right approach.
--
-- Beyond the HTTP status code shenanigans this function returns the message as
-- plain text. It also calls 'Scotty.finish' so nothing else will happen.
bail :: String -> Scotty.ActionM a
bail message = do
  Scotty.status Http.ok200
  Scotty.text $ LazyText.pack message
  Scotty.finish

checkSlackSignature
  :: SlackSigningSecret -> LazyByteString.ByteString -> Scotty.ActionM ()
checkSlackSignature secret body = do
  timestamp <- getTimestamp
  digest <- getDigest
  let message = LazyByteString.toStrict $ "v0:" <> timestamp <> ":" <> body
  Monad.when (Crypto.HMAC digest /= Crypto.hmac secret message)
    . bail
    $ "I refused to handle the request from Slack because the signature was "
    <> "invalid."

getTimestamp :: Scotty.ActionM LazyByteString.ByteString
getTimestamp = do
  lazyText <- requireHeader "X-Slack-Request-Timestamp"
  pure $ LazyText.encodeUtf8 lazyText

getDigest :: Scotty.ActionM (Crypto.Digest Crypto.SHA256)
getDigest = do
  signature <- requireHeader "X-Slack-Signature"
  let prefix = "v0=" :: LazyText.Text
  base16 <- case LazyText.stripPrefix prefix signature of
    Nothing ->
      bail
        $ "I could not understand the request from Slack because the "
        <> "signature ("
        <> show signature
        <> ") was missing the required prefix ("
        <> LazyText.unpack prefix
        <> ")."
    Just base16 -> pure base16
  byteString <- case fromBase16 base16 of
    Left _ ->
      bail
        $ "I could not understand the request from Slack because the "
        <> "signature ("
        <> show signature
        <> ") was not valid hexadecimal."
    Right byteString -> pure byteString
  case Crypto.digestFromByteString byteString of
    Nothing ->
      bail
        $ "I could not understand the request from Slack because the "
        <> "signature ("
        <> show signature
        <> ") was not a valid SHA256 digest."
    Just digest -> pure digest

requireHeader :: LazyText.Text -> Scotty.ActionM LazyText.Text
requireHeader name = do
  maybeValue <- Scotty.header name
  case maybeValue of
    Nothing ->
      bail
        $ "I could not understand the request from Slack because it was "
        <> "missing the required "
        <> LazyText.unpack name
        <> " HTTP header."
    Just value -> pure value

fromBase16 :: LazyText.Text -> Either String ByteString.ByteString
fromBase16 =
  Memory.convertFromBase Memory.Base16
    . LazyByteString.toStrict
    . LazyText.encodeUtf8

getWorkoutsAction :: Sql.Connection -> Scotty.ActionM ()
getWorkoutsAction connection = do
  Time.ZonedTime _ zone <- Scotty.liftAndCatchIO Time.getZonedTime
  users <- Scotty.liftAndCatchIO $ selectUsers connection
  workouts <- Scotty.liftAndCatchIO $ selectWorkouts connection zone
  let rows = makeRows users workouts
  Scotty.setHeader "Content-Type" "text/csv; charset=utf-8"
  Scotty.raw $ Csv.encodeDefaultOrderedByName rows

makeRows :: [User] -> [Workout Time.ZonedTime] -> [Row]
makeRows users =
  let
    usersById = indexBy userId users
    makeRow workout = do
      user <- Map.lookup (workoutUserId workout) usersById
      Just Row { rowUser = user, rowWorkout = workout }
  in Maybe.mapMaybe makeRow
    . List.sortOn (Time.zonedTimeToUTC . workoutRecordedAt)

data Row = Row
  { rowUser :: User
  , rowWorkout :: Workout Time.ZonedTime
  } deriving (Show)

instance Csv.DefaultOrdered Row where
  headerOrder = const . Vector.fromList $ fmap fst rowCsvFields

instance Csv.ToNamedRecord Row where
  toNamedRecord row = Csv.namedRecord
    $ fmap (\(name, toField) -> (name, toField row)) rowCsvFields

rowCsvFields :: [(Csv.Name, Row -> Csv.Field)]
rowCsvFields =
  [ ("Workout ID", Csv.toField . workoutId . rowWorkout)
  , ( "Recorded at"
    , Csv.toField
      . formatTime "%Y-%m-%dT%H:%M:%S%Q%z"
      . workoutRecordedAt
      . rowWorkout
    )
  , ("Exercise", Csv.toField . workoutExercise . rowWorkout)
  , ("User ID", Csv.toField . userId . rowUser)
  , ("User name", Csv.toField . userName . rowUser)
  , ("count", Csv.toField . workoutCount . rowWorkout)
  ]

notFoundAction :: Scotty.ActionM ()
notFoundAction = Scotty.text "404 Not Found"

migrations :: [Sql.Query]
migrations =
  [ [Sql.sql|
      create table if not exists users
      ( id text primary key
      , name text not null
      )
    |]
  , [Sql.sql|
      create index if not exists users_name_index
      on users ( name )
    |]
  , [Sql.sql|
      create table if not exists workouts
      ( id text primary key
      , recorded_at text not null
      , exercise text not null
      , user_id text not null
      , count int not null
      )
    |]
  , [Sql.sql|
      create index if not exists workouts_recorded_at_index
      on workouts ( recorded_at )
    |]
  , [Sql.sql|
      create index if not exists workouts_exercise_index
      on workouts ( exercise )
    |]
  , [Sql.sql|
      create index if not exists workouts_user_id_index
      on workouts ( user_id )
    |]
  , [Sql.sql|
      create index if not exists workouts_count_index
      on workouts ( count )
    |]
  ]

data Config = Config
  { configDatabaseFile :: FilePath
  , configServerHost :: Warp.HostPreference
  , configServerPort :: Warp.Port
  , configSlackSigningSecret :: SlackSigningSecret
  } deriving (Eq, Show)

getConfig :: IO Config
getConfig = do
  configDatabaseFile <- getDatabaseFile
  configServerHost <- getServerHost
  configServerPort <- getServerPort
  configSlackSigningSecret <- getSlackSigningSecret
  pure Config
    { configDatabaseFile
    , configServerHost
    , configServerPort
    , configSlackSigningSecret
    }

getDatabaseFile :: IO FilePath
getDatabaseFile = do
  maybeString <- Environment.lookupEnv "DATABASE_FILE"
  pure $ Maybe.fromMaybe ":memory:" maybeString

getServerHost :: IO Warp.HostPreference
getServerHost = do
  maybeString <- Environment.lookupEnv "SERVER_HOST"
  pure $ case maybeString of
    Nothing -> "127.0.0.1"
    Just string -> String.fromString string

getServerPort :: IO Warp.Port
getServerPort = do
  maybeString <- Environment.lookupEnv "SERVER_PORT"
  case maybeString of
    Nothing -> pure 8080
    Just string -> case Read.readMaybe string of
      Nothing -> fail $ "invalid Port: " <> show string
      Just port -> pure port

getSlackSigningSecret :: IO SlackSigningSecret
getSlackSigningSecret = do
  string <- Environment.getEnv "SLACK_SIGNING_SECRET"
  pure $ stringToSlackSigningSecret string

configToOptions :: Config -> Scotty.Options
configToOptions config =
  updateSettings Default.def
    $ Warp.setHost (configServerHost config)
    . Warp.setOnExceptionResponse onExceptionResponse
    . Warp.setPort (configServerPort config)
    . Warp.setServerName serverName

updateSettings
  :: Scotty.Options -> (Warp.Settings -> Warp.Settings) -> Scotty.Options
updateSettings options update =
  options { Scotty.settings = update $ Scotty.settings options }

onExceptionResponse :: Exception.SomeException -> Wai.Response
onExceptionResponse _ = Wai.responseLBS
  Http.internalServerError500
  [(Http.hContentType, "text/plain; charset=utf-8")]
  "500 Internal Server Error"

serverName :: ByteString.ByteString
serverName =
  Text.encodeUtf8
    . Text.pack
    $ "arnold/"
    <> Version.showVersion Package.version

newtype Rank = Rank
  { unwrapRank :: Natural.Natural
  } deriving (Eq, Show)

instance Semigroup Rank where
  x <> y = Rank $ unwrapRank x + unwrapRank y

instance Lucid.ToHtml Rank where
  toHtml = toHtmlViaRaw
  toHtmlRaw = Lucid.toHtmlRaw . show . unwrapRank

rankBy :: Ord b => (a -> b) -> [a] -> [(Rank, a)]
rankBy f =
  fst
    . foldl
        (\(ys, rank) ties ->
          ( ys <> zip (repeat rank) (fmap snd ties)
          , rank <> Rank (List.genericLength ties)
          )
        )
        ([], Rank 1)
    . List.groupBy (\x y -> fst x == fst y)
    . List.sortOn fst
    . fmap (\x -> (f x, x))

newtype Score = Score
  { unwrapScore :: Natural.Natural
  } deriving (Eq, Ord, Show)

instance Monoid Score where
  mempty = Score 0

instance Semigroup Score where
  x <> y = Score $ unwrapScore x + unwrapScore y

instance Lucid.ToHtml Score where
  toHtml = toHtmlViaRaw
  toHtmlRaw = Lucid.toHtmlRaw . show . unwrapScore

newtype SlackSigningSecret = SlackSigningSecret
  { unwrapSlackSigningSecret :: Memory.ScrubbedBytes
  } deriving (Eq, Show)

instance Memory.ByteArrayAccess SlackSigningSecret where
  length = Memory.length . unwrapSlackSigningSecret
  withByteArray = Memory.withByteArray . unwrapSlackSigningSecret

stringToSlackSigningSecret :: String -> SlackSigningSecret
stringToSlackSigningSecret =
  SlackSigningSecret . Memory.convert . Text.encodeUtf8 . Text.pack

data Payload = Payload
  { payloadCommand :: Text.Text
  , payloadResponseUrl :: Text.Text
  , payloadText :: Text.Text
  , payloadUserId :: UserId
  , payloadUserName :: Name
  } deriving (Eq, Show)

instance Form.FromForm Payload where
  fromForm form = do
    payloadCommand <- Form.parseUnique "command" form
    payloadResponseUrl <- Form.parseUnique "response_url" form
    payloadText <- Form.parseUnique "text" form
    payloadUserId <- Form.parseUnique "user_id" form
    payloadUserName <- Form.parseUnique "user_name" form
    pure Payload
      { payloadCommand
      , payloadResponseUrl
      , payloadText
      , payloadUserId
      , payloadUserName
      }

data User = User
  { userId :: UserId
  , userName :: Name
  } deriving (Eq, Show)

instance Sql.FromRow User where
  fromRow = do
    userId <- Sql.field
    userName <- Sql.field
    pure User { userId, userName }

instance Sql.ToRow User where
  toRow user =
    fmap (\f -> f user) [Sql.toField . userId, Sql.toField . userName]

newtype Name = Name
  { unwrapName :: Text.Text
  } deriving (Eq, Show)

instance Sql.FromField Name where
  fromField field = do
    text <- Sql.fromField field
    pure $ Name text

instance Form.FromHttpApiData Name where
  parseUrlPiece urlPiece = do
    text <- Form.parseUrlPiece urlPiece
    pure $ Name text

instance Csv.ToField Name where
  toField = Csv.toField . unwrapName

instance Sql.ToField Name where
  toField = Sql.toField . unwrapName

newtype WorkoutId = WorkoutId
  { unwrapWorkoutId :: Uuid.UUID
  } deriving (Eq, Show)

instance Sql.FromField WorkoutId where
  fromField field = do
    text <- Sql.fromField field
    case Uuid.fromText text of
      Nothing -> fail $ "invalid WorkoutId: " <> show text
      Just uuid -> pure $ WorkoutId uuid

instance Csv.ToField WorkoutId where
  toField = Csv.toField . Uuid.toText . unwrapWorkoutId

instance Sql.ToField WorkoutId where
  toField = Sql.toField . Uuid.toText . unwrapWorkoutId

newtype Count = Count
  { unwrapCount :: Natural.Natural
  } deriving (Eq, Ord, Show)

instance Sql.FromField Count where
  fromField field = do
    integer <- Sql.fromField field
    case integerToNatural integer of
      Nothing -> fail $ "invalid Count: " <> show integer
      Just natural -> pure $ Count natural

instance Monoid Count where
  mempty = Count 0

instance Semigroup Count where
  x <> y = Count $ unwrapCount x + unwrapCount y

instance Csv.ToField Count where
  toField = Csv.toField . unwrapCount

instance Sql.ToField Count where
  toField = Sql.toField . naturalToInteger . unwrapCount

instance Lucid.ToHtml Count where
  toHtml = toHtmlViaRaw
  toHtmlRaw = Lucid.toHtmlRaw . show . unwrapCount

newtype UserId = UserId
  { unwrapUserId :: Text.Text
  } deriving (Eq, Ord, Show)

instance Sql.FromField UserId where
  fromField field = do
    text <- Sql.fromField field
    pure $ UserId text

instance Form.FromHttpApiData UserId where
  parseUrlPiece urlPiece = do
    text <- Form.parseUrlPiece urlPiece
    pure $ UserId text

instance Csv.ToField UserId where
  toField = Csv.toField . unwrapUserId

instance Sql.ToField UserId where
  toField = Sql.toField . unwrapUserId

-- | This type is polymorphic in the time so that it can be used with both
-- 'Time.UTCTime' and 'Time.ZonedTime'.
data Workout time = Workout
  { workoutId :: WorkoutId
  , workoutRecordedAt :: time
  , workoutExercise :: Exercise
  , workoutUserId :: UserId
  , workoutCount :: Count
  } deriving (Eq, Show)

instance Csv.DefaultOrdered (Workout time) where
  headerOrder = const . Vector.fromList $ fmap
    fst
    (workoutCsvFields :: [(Csv.Name, Workout Time.ZonedTime -> Csv.Field)])

instance Sql.FromField time => Sql.FromRow (Workout time) where
  fromRow = do
    workoutId <- Sql.field
    workoutRecordedAt <- Sql.field
    workoutExercise <- Sql.field
    workoutUserId <- Sql.field
    workoutCount <- Sql.field
    pure Workout
      { workoutId
      , workoutRecordedAt
      , workoutExercise
      , workoutUserId
      , workoutCount
      }

instance Sql.ToField time => Sql.ToRow (Workout time) where
  toRow workout = fmap
    (\f -> f workout)
    [ Sql.toField . workoutId
    , Sql.toField . workoutRecordedAt
    , Sql.toField . workoutExercise
    , Sql.toField . workoutUserId
    , Sql.toField . workoutCount
    ]

instance Time.FormatTime time => Csv.ToNamedRecord (Workout time) where
  toNamedRecord workout = Csv.namedRecord
    $ fmap (\(name, toField) -> (name, toField workout)) workoutCsvFields

workoutCsvFields
  :: Time.FormatTime time => [(Csv.Name, Workout time -> Csv.Field)]
workoutCsvFields =
  [ ("id", Csv.toField . workoutId)
  , ( "recorded_at"
    , Csv.toField . formatTime "%Y-%m-%dT%H:%M:%S%Q%z" . workoutRecordedAt
    )
  , ("exercise", Csv.toField . workoutExercise)
  , ("user_id", Csv.toField . workoutUserId)
  , ("count", Csv.toField . workoutCount)
  ]

data Exercise
  = ExercisePlank
  | ExercisePushUp
  | ExerciseSquat
  deriving (Eq, Ord, Show)

instance Sql.FromField Exercise where
  fromField field = do
    string <- Sql.fromField field
    case stringToExercise string of
      Nothing -> fail $ "invalid Exercise: " <> show string
      Just exercise -> pure exercise

instance Csv.ToField Exercise where
  toField = Csv.toField . exerciseToString

instance Sql.ToField Exercise where
  toField = Sql.toField . exerciseToString

instance Lucid.ToHtml Exercise where
  toHtml = toHtmlViaRaw
  toHtmlRaw = Lucid.toHtmlRaw . exerciseToString

exerciseToString :: Exercise -> String
exerciseToString exercise = case exercise of
  ExercisePlank -> "plank"
  ExercisePushUp -> "push-up"
  ExerciseSquat -> "squat"

stringToExercise :: String -> Maybe Exercise
stringToExercise string = case string of
  "plank" -> Just ExercisePlank
  "push-up" -> Just ExercisePushUp
  "squat" -> Just ExerciseSquat
  _ -> Nothing

-- | This is like 'stringToExercise' except that it's way more lenient.
parseExercise :: String -> Maybe Exercise
parseExercise string = case words string of
  ["plank"] -> Just ExercisePlank
  ["planks"] -> Just ExercisePlank
  ["push", "up"] -> Just ExercisePushUp
  ["push", "ups"] -> Just ExercisePushUp
  ["push-up"] -> Just ExercisePushUp
  ["push-ups"] -> Just ExercisePushUp
  ["pushup"] -> Just ExercisePushUp
  ["pushups"] -> Just ExercisePushUp
  ["squat"] -> Just ExerciseSquat
  ["squats"] -> Just ExerciseSquat
  _ -> Nothing

formatTime :: Time.FormatTime time => String -> time -> String
formatTime = Time.formatTime Time.defaultTimeLocale

groupBy :: (Foldable t, Ord k) => (a -> k) -> t a -> Map.Map k [a]
groupBy f = foldr (\x -> Map.alter (Just . maybe [x] (x :)) (f x)) Map.empty

indexBy :: (Foldable t, Ord k) => (a -> k) -> t a -> Map.Map k a
indexBy f = foldr (\x -> Map.insert (f x) x) Map.empty

integerToNatural :: Integer -> Maybe Natural.Natural
integerToNatural integer =
  if integer < 0 then Nothing else Just $ fromIntegral integer

naturalToInteger :: Natural.Natural -> Integer
naturalToInteger = fromIntegral

toHtmlViaRaw :: (Monad m, Lucid.ToHtml a) => a -> Lucid.HtmlT m ()
toHtmlViaRaw = Lucid.toHtml . Lucid.toHtmlRaw
