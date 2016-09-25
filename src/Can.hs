{-# LINE 1 "src/Can.hsc" #-}
module Can where
{-# LINE 2 "src/Can.hsc" #-}

import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Ptr
import Data.Bits


{-# LINE 10 "src/Can.hsc" #-}


{-# LINE 12 "src/Can.hsc" #-}

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
  sizeOf _  = 8
{-# LINE 30 "src/Can.hsc" #-}
  alignment _ = (16)
{-# LINE 31 "src/Can.hsc" #-}
  peek ptr  = do
    id    <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 33 "src/Can.hsc" #-}
    dlc   <- ((\hsc_ptr -> peekByteOff hsc_ptr 4) ptr) :: IO CUChar
{-# LINE 34 "src/Can.hsc" #-}
    pad   <- (\hsc_ptr -> peekByteOff hsc_ptr 5) ptr
{-# LINE 35 "src/Can.hsc" #-}
    res0  <- (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr
{-# LINE 36 "src/Can.hsc" #-}
    res1  <- (\hsc_ptr -> peekByteOff hsc_ptr 7) ptr
{-# LINE 37 "src/Can.hsc" #-}
    data' <- peekArray (fromIntegral dlc) ((\hsc_ptr -> hsc_ptr `plusPtr` 8) ptr)
{-# LINE 38 "src/Can.hsc" #-}
    return $ CanFrame id dlc pad res0 res1 data'
  poke ptr (CanFrame id dlc pad res0 res1 data') = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr id
{-# LINE 41 "src/Can.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr dlc
{-# LINE 42 "src/Can.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 5) ptr pad
{-# LINE 43 "src/Can.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr res0
{-# LINE 44 "src/Can.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 7) ptr res1
{-# LINE 45 "src/Can.hsc" #-}
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 8) ptr) data'
{-# LINE 46 "src/Can.hsc" #-}

data CanFilter = CanFilter
  { _canFilterCanId   :: CInt
  , _canFilterCanMask :: CInt
  } deriving Show

instance Storable CanFilter where
  sizeOf _  = 4
{-# LINE 54 "src/Can.hsc" #-}
  alignment _ = (8)
{-# LINE 55 "src/Can.hsc" #-}
  peek ptr = do
    id <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 57 "src/Can.hsc" #-}
    mask <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 58 "src/Can.hsc" #-}
    return $ CanFilter id mask
  poke ptr (CanFilter id mask) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr id
{-# LINE 61 "src/Can.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr mask
{-# LINE 62 "src/Can.hsc" #-}

isErrorMessage :: CanFrame -> Bool
isErrorMessage cf = testBit (_canFrameCanId cf) 29
{-# LINE 65 "src/Can.hsc" #-}

isExtendedFrameFormat :: CanFrame -> Bool
isExtendedFrameFormat cf = testBit (_canFrameCanId cf) 11
{-# LINE 68 "src/Can.hsc" #-}

isStandardFrameFormat :: CanFrame -> Bool
isStandardFrameFormat cf = not $ isExtendedFrameFormat cf
