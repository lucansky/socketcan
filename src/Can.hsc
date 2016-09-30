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
  poke ptr (CanFrame id _ _ _ dlc pad res0 res1 data') = do
    #{poke struct can_frame, can_id} ptr id
    #{poke struct can_frame, can_dlc} ptr dlc
    #{poke struct can_frame, __pad} ptr pad
    #{poke struct can_frame, __res0} ptr res0
    #{poke struct can_frame, __res1} ptr res1
    pokeArray (#{ptr struct can_frame, data} ptr) data'

data CanFdFrame = CanFdFrame
  { -- | 32 bit CAN_ID + EFF/RTR/ERR flags
    _canFdFrameCanId :: CanId
    -- | Extended Frame Format Flag
  , _canFdFrameEFF    :: Bool
  -- | Error Message Frame Flag
  , _canFdFrameERR    :: Bool
  -- | Remote Transmission Request Flag
  , _canFdFrameRTR    :: Bool
  -- | frame payload length in byte
  , _canFdFrameLen   :: CUChar
  -- | additional flags for CAN FD
  , _canFdFrameFlags :: CUChar
  -- | reserved / padding
  , _canFdFrameRes0  :: CUChar
  -- | reserved / padding
  , _canFdFrameRes1  :: CUChar
  -- | CAN FD frame payload (up to CANFD_MAX_DLEN byte)
  , _canFdFrameData  :: [CUChar]
  }

instance Storable CanFdFrame where
  sizeOf _  = #{alignment struct canfd_frame}
  alignment _ = #{size struct canfd_frame}
  peek ptr  = do
    id    <- #{peek struct canfd_frame, can_id} ptr
    len   <- (#{peek struct canfd_frame, len} ptr) :: IO CUChar
    flags   <- (#{peek struct canfd_frame, flags} ptr) :: IO CUChar
    res0  <- #{peek struct canfd_frame, __res0} ptr
    res1  <- #{peek struct canfd_frame, __res1} ptr
    data' <- peekArray (fromIntegral len) (#{ptr struct canfd_frame, data} ptr)
    return $ CanFdFrame (maskId id) (isEff id) (isErr id) (isRtr id)len flags res0 res1 data'
  poke ptr (CanFdFrame id _ _ _ len flags res0 res1 data') = do
    #{poke struct canfd_frame, can_id} ptr id
    #{poke struct canfd_frame, len} ptr len
    #{poke struct canfd_frame, flags} ptr flags
    #{poke struct canfd_frame, __res0} ptr res0
    #{poke struct canfd_frame, __res1} ptr res1
    pokeArray (#{ptr struct canfd_frame, data} ptr) data'

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

maskId :: CUInt -> CanId
maskId canId
  | isEff canId = (canId .&. #{const CAN_EFF_MASK})
  | otherwise   = (canId .&. #{const CAN_SFF_MASK})

isEff :: CanId -> Bool
isEff canId = (canId .&. #{const CAN_EFF_FLAG}) /= 0

isErr :: CanId -> Bool
isErr canId = (canId .&. #{const CAN_ERR_FLAG}) /= 0

isRtr :: CanId -> Bool
isRtr canId = (canId .&. #{const CAN_RTR_FLAG}) /= 0

