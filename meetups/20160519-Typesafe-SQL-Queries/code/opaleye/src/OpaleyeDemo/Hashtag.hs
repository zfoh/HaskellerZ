{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module OpaleyeDemo.Hashtag where

import           Control.Arrow              (returnA)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Opaleye                    (Query, Table (..), pgInt4,
                                             queryTable, required, restrict,
                                             (.==))
import           OpaleyeDemo.Ids

--------------------------------------------------------------------------------
-- | Hashtag declaration
data Hashtag' i h = Hashtag { _todoId  :: i
                            , _hashtag :: h
                            } deriving Show
makeAdaptorAndInstance "pHashtag" ''Hashtag'

--------------------------------------------------------------------------------
-- | Hashtag Columns
type HashtagColumns
  = Hashtag' TodoIdColumn HashtagStrColumn
type HashtagInsertColumns
  = Hashtag' TodoIdColumn HashtagStrColumn
type HashtagNullableColumns
  = Hashtag' TodoIdColumnNullable HashtagStrColumnNullable
type Hashtag
  = Hashtag' TodoId HashtagStr

--------------------------------------------------------------------------------
-- | Functions
hashtagTable :: Table HashtagInsertColumns HashtagColumns
hashtagTable = Table "hashtags" $ pHashtag Hashtag
    { _todoId  = pTodoId     . TodoId     $ required "todo_id"
    , _hashtag = pHashtagStr . HashtagStr $ required "hashtag"
    }

hashtagQuery :: Query HashtagColumns
hashtagQuery = queryTable hashtagTable

hashtagsForTodo :: TodoId -> Query HashtagColumns
hashtagsForTodo tid = proc () -> do
    hashtags <- hashtagQuery -< ()
    restrict -< todoId (_todoId hashtags) .== pgInt4 (todoId tid)
    returnA  -< hashtags

