module Can where

import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Bits

#include "can.h"

#let alignment t = "%lu", (unsigned long) offsetof(struct { char x__; t (y__); }, y__)

type CanId = CUInt

data CanFrame = CanFrame
  { -- | (29bit) 32 bit CAN_ID excluding EFF/RTR/ERR flags
    _canFrameCanId  :: CanId
  -- | Extended Frame Format Flag
  , _canFrameEFF    :: Bool
  -- | Error Message Frame Flag
  , _canFrameERR    :: Bool
  -- | Remote Transmission Request Flag
  , _canFrameRTR    :: Bool
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
    return $ CanFrame (maskId id) (isEff id) (isErr id) (isRtr id) dlc pad res0 res1 data'
    where errFlag = #{const CAN_ERR_FLAG}
          rtrFlag = #{const CAN_RTR_FLAG}
          effFlag  = #{const CAN_EFF_FLAG}
          maskId canId  = (canId .&. #{const CAN_SFF_MASK})
          isEff canId = (canId .&. effFlag) /= 0
          isErr canId = (canId .&. errFlag) /= 0
          isRtr canId = (canId .&. rtrFlag) /= 0
  poke ptr (CanFrame id _ _ _ dlc pad res0 res1 data') = do
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

