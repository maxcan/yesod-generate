{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Root where

import Import

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = 
    defaultLayout $ do
        h2id <- lift newIdent
        setTitle "GenTrial homepage"
        $(widgetFile "homepage")


getAddUserR :: Text -> Text -> Handler ()
getAddUserR u p = do
  runDB $ insert $ User u (Just p)
  setMessage "added user"
  redirect RootR


optionsPersistKey
  :: (YesodPersist master
     , PersistEntity a
     , PersistQuery (YesodPersistBackend master) (GHandler sub master)
     , PathPiece (Key (YesodPersistBackend master) a)
     , RenderMessage master msg
     , PersistEntityBackend a ~ YesodPersistBackend master) 
  => [Filter a] 
  -> [SelectOpt a] 
  -> (Entity a -> msg) 
  -> GHandler sub master (OptionList (Key (PersistEntityBackend a) a))
optionsPersistKey filts ords toDisplay = fmap mkOptionList $ do
    mr <- getMessageRender
    pairs <- runDB $ selectList filts ords
    return $ map (\(Entity key value) -> Option
        { optionDisplay = mr (toDisplay $ Entity key value)
        , optionInternalValue = key
        , optionExternalValue = toPathPiece key
        }) pairs

fooForm :: Maybe Foo -> Form Foo
fooForm mFoo = 
  renderDivs $ Foo
    <$> aopt textField "bar" (fooBar <$> mFoo)
    <*> areq (selectField (optionsPersistKey [] [Asc UserIdent] (toPathPiece . entityKey))) "user" (fooUser <$> mFoo)
    <*> areq intField "age" (fooAge <$> mFoo)

getFooR           :: Handler RepHtml
postFooR          :: Handler RepHtml
getFooNewR        :: Handler RepHtml
getFooDetailR     :: FooId -> Handler RepHtml
putFooDetailR     :: FooId -> Handler RepHtml
deleteFooDetailR  :: FooId -> Handler RepHtml
getFooEditR       :: FooId -> Handler RepHtml


getFooR = do
  (allFoos :: [Entity Foo]) <- runDB $ selectList [] []
  defaultLayout $ $(widgetFile "foo/index")

postFooR = do
  ((formResult, formWidget), enctype) <- runFormPost $ fooForm Nothing
  case formResult of
    FormSuccess foo -> do
      fooId <- runDB $ insert foo
      setMessage "successfully added"
      redirect $ FooDetailR fooId
    _ -> do 
      setMessage "form error"
      redirect FooNewR
    
getFooNewR = do
  ((_, formWidget), enctype) <- generateFormPost $ fooForm Nothing
  defaultLayout $ $(widgetFile "foo/new")
    
  
getFooDetailR fooId = do
  foo <- runDB $ get404 fooId
  defaultLayout $ $(widgetFile "foo/show")
putFooDetailR fooId = undefined
deleteFooDetailR fooId = undefined 
getFooEditR fooId = undefined
