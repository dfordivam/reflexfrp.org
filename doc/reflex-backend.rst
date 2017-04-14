Using Reflex in Backend
=======================

Design
------

The server' internals (the business logic) is implemented in reflex world.
  I call this its own *world* because, once it is init there are limited number of operations one can do,
  namely triggering events (to send it data) and doing some IO based on events (basically equivalent of receiving data).

The commmunication with network is done with usual APIs Wai/Servent.
There are handlers between reflex world and the outside, which fetch DB
data, create and perform ``Event``s which require network communication.

The outside world is multi-threaded environment with many connections and possibly many DBs.
The core logic is in reflex which runs in its own timeline, so it is inherently serial??
It also means there should be no need for any explicit MVar/TVar.

Can this architecture scale?

Motivation
----------

May be this architecture simplifies the complex data flows, especially in SNS where the server need to
move data here and there based on some rules. In a typical REST kind of app I dont see much merit.


Twitter example
---------------

An example to illustrate the design::

  -- Instead of MVar can we just use Dynamic
  -- Are Dynamic values thread-safe

  data App {
      serverData :: TVar TwitterServerData
  }

  data TwitterServerData {
      users :: Dynamic t (Map UserId User)
    , activeFeeds :: Dynamic t [UserId]
  }

  data User {
      tweets :: Dynamic t (Map TweetId Tweet)
    , subscribers :: Dynamic t [UserId]
  }

  data Tweet {
      content :: Text
    , likes :: Dynamic t [UserId]
  }

  -- The interface Req and Resp dont contain Reflex stuff
  data Request =
      AddUser User
    | PostTweet UserId Tweet
    | Follow UserId UserId
    | LikeTweet TweetId
    | GetFeed UserId

  data Response =
      Feed [Tweet]

  response :: Dynamic t Response
  serverHandleRequest :: Event t Request -> m (Dynamic t Response)
  serverHandleRequest req =
    serverDataDyn <- foldDyn handleRequest (initData) req
    where
      handleRequest req prevSt =
        case request of
          (AddUser User) ->

      getUserFeed :: UserId -> [Tweet]
      getUserFeed uId =

  websocket response

  -- A server require to serve the connections concurrently.
  -- There will be multiple threads executions.
  -- Each user has independent WS connection and handler running in server
  mainWSHandler :: Conduit Request -> m (Conduit Response)
  mainWSHandler req = do
    -- Independent processes/conduits
    -- Response can be generated/pushed even without a request.
    req ~> handleReq
    source getResponse

    handleReq :: Request -> m ()
    getResponse :: m (Response)

    -- Interface to reflex world
    -- How to get reflex timeline? share it?
    -- In the start the reflex graph will be created with callbacks from external world
    -- App will contain ``addUser :: (User -> IO ())`` which will trigger an add Event in reflex.
    -- The Dynamic will have a performEvent load which will trigger the response.
    --   This call back routine has to be provided from outside.
    --   respFun :: Response -> IO ()
    --   performEvent (respFun <$> resp)
    --
    --   This will create appropriate things in reflex to provide response
    --   addRespFun :: UserId -> (Response -> IO ()) -> IO ()



    Dynamic t [Tweet] == Dynamic t Response
