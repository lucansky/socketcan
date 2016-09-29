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
  sizeOf _  = 8
{-# LINE 38 "src/Can.hsc" #-}
  alignment _ = (16)
{-# LINE 39 "src/Can.hsc" #-}
  peek ptr  = do
    id    <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 41 "src/Can.hsc" #-}
    dlc   <- ((\hsc_ptr -> peekByteOff hsc_ptr 4) ptr) :: IO CUChar
{-# LINE 42 "src/Can.hsc" #-}
    pad   <- (\hsc_ptr -> peekByteOff hsc_ptr 5) ptr
{-# LINE 43 "src/Can.hsc" #-}
    res0  <- (\hsc_ptr -> peekByteOff hsc_ptr 6) ptr
{-# LINE 44 "src/Can.hsc" #-}
    res1  <- (\hsc_ptr -> peekByteOff hsc_ptr 7) ptr
{-# LINE 45 "src/Can.hsc" #-}
    data' <- peekArray (fromIntegral dlc) ((\hsc_ptr -> hsc_ptr `plusPtr` 8) ptr)
{-# LINE 46 "src/Can.hsc" #-}
    return $ CanFrame (maskId id) (isEff id) (isErr id) (isRtr id) dlc pad res0 res1 data'
    where errFlag = 536870912
{-# LINE 48 "src/Can.hsc" #-}
          rtrFlag = 1073741824
{-# LINE 49 "src/Can.hsc" #-}
          effFlag  = 2147483648
{-# LINE 50 "src/Can.hsc" #-}
          maskId canId  = (canId .&. 2047)
{-# LINE 51 "src/Can.hsc" #-}
          isEff canId = (canId .&. effFlag) /= 0
          isErr canId = (canId .&. errFlag) /= 0
          isRtr canId = (canId .&. rtrFlag) /= 0
  poke ptr (CanFrame id _ _ _ dlc pad res0 res1 data') = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr id
{-# LINE 56 "src/Can.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr dlc
{-# LINE 57 "src/Can.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 5) ptr pad
{-# LINE 58 "src/Can.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 6) ptr res0
{-# LINE 59 "src/Can.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 7) ptr res1
{-# LINE 60 "src/Can.hsc" #-}
    pokeArray ((\hsc_ptr -> hsc_ptr `plusPtr` 8) ptr) data'
{-# LINE 61 "src/Can.hsc" #-}

data CanFilter = CanFilter
  { _canFilterCanId   :: CInt
  , _canFilterCanMask :: CInt
  } deriving Show

instance Storable CanFilter where
  sizeOf _  = 4
{-# LINE 69 "src/Can.hsc" #-}
  alignment _ = (8)
{-# LINE 70 "src/Can.hsc" #-}
  peek ptr = do
    id <- (\hsc_ptr -> peekByteOff hsc_ptr 0) ptr
{-# LINE 72 "src/Can.hsc" #-}
    mask <- (\hsc_ptr -> peekByteOff hsc_ptr 4) ptr
{-# LINE 73 "src/Can.hsc" #-}
    return $ CanFilter id mask
  poke ptr (CanFilter id mask) = do
    (\hsc_ptr -> pokeByteOff hsc_ptr 0) ptr id
{-# LINE 76 "src/Can.hsc" #-}
    (\hsc_ptr -> pokeByteOff hsc_ptr 4) ptr mask
{-# LINE 77 "src/Can.hsc" #-}

isErrorMessage :: CanId -> Bool
isErrorMessage canId = testBit canId 29
{-# LINE 80 "src/Can.hsc" #-}

isExtendedFrameFormat :: CanId -> Bool
isExtendedFrameFormat canId = testBit canId 11
{-# LINE 83 "src/Can.hsc" #-}

isStandardFrameFormat :: CanId -> Bool
isStandardFrameFormat canId = not $ isExtendedFrameFormat canId
