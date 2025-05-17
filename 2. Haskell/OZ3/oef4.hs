{-
    1. IO Recursion (EXTRA)
-}

loop :: a -> (a -> IO a) -> IO ()
loop val f = do
  new <- f val
  loop new f

-- bij inc is a==Int
inc :: Int -> IO Int
inc x = do
  let y = x + 1 -- !! let !! want geen IO Int als resultaat => Dus we kunnen niet binden (>>=) ~ (<-)
  print y -- print werkt voor instances van Show (dat is het geval voor Int)
  return y -- zet monad shell rond y::Int => (IO Int)