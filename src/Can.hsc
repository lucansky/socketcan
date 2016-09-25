module Can where

import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Bits

#include "can.h"

#let alignment t = "%lu", (unsigned long) offsetof(struct { char x__; t (y__); }, y__)

data CanFrame = CanFrame
  { -- | 32 bit CAN_ID + EFF/RTR/ERR flags
    _canFrameCanId  :: CUInt
  -- | frame payload length in byte (0 .. CAN_MAX_DLEN)
  , _canFrameCanDlc :: CUChar
  -- | padding
  , _canFramePad    :: CUChar
  -- | reserved padding
  , _canFrameRes0   :: CUChar
  -- | reserved padding
  , _canFrameRes1   :: CUChar
  -- | CAN frame payload (up to 8 byte)
  , _canFrameData   :: [CUChar]
  } deriving Show

instance Storable CanFrame where
  sizeOf _  = #{alignment struct can_frame}
  alignment _ = #{size struct can_frame}
  peek ptr  = do
    id    <- #{peek struct can_frame, can_id} ptr
    dlc   <- (#{peek struct can_frame, can_dlc} ptr) :: IO CUChar
    pad   <- #{peek struct can_frame, __pad} ptr
    res0  <- #{peek struct can_frame, __res0} ptr
    res1  <- #{peek struct can_frame, __res1} ptr
    data' <- peekArray (fromIntegral dlc) (#{ptr struct can_frame, data} ptr)
    return $ CanFrame id dlc pad res0 res1 data'
  poke ptr (CanFrame id dlc pad res0 res1 data') = do
    #{poke struct can_frame, can_id} ptr id
    #{poke struct can_frame, can_dlc} ptr dlc
    #{poke struct can_frame, __pad} ptr pad
    #{poke struct can_frame, __res0} ptr res0
    #{poke struct can_frame, __res1} ptr res1
    pokeArray (#{ptr struct can_frame, data} ptr) data'

data CanFilter = CanFilter
  { _canFilterCanId   :: CInt
  , _canFilterCanMask :: CInt
  } deriving Show

instance Storable CanFilter where
  sizeOf _  = #{alignment struct can_filter}
  alignment _ = #{size struct can_filter}
  peek ptr = do
    id <- #{peek struct can_filter, can_id} ptr
    mask <- #{peek struct can_filter, can_mask} ptr
    return $ CanFilter id mask
  poke ptr (CanFilter id mask) = do
    #{poke struct can_filter, can_id} ptr id
    #{poke struct can_filter, can_mask} ptr mask

isErrorMessage :: CanFrame -> Bool
isErrorMessage cf = testBit (_canFrameCanId cf) #{const CAN_EFF_ID_BITS}

isExtendedFrameFormat :: CanFrame -> Bool
isExtendedFrameFormat cf = testBit (_canFrameCanId cf) #{const CAN_SFF_ID_BITS}

isStandardFrameFormat :: CanFrame -> Bool
isStandardFrameFormat cf = not $ isExtendedFrameFormat cf
