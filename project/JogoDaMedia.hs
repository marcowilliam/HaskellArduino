module System.Hardware.Arduino.SamplePrograms.Servo where

import Control.Monad       (forever)
import Control.Monad.Trans (liftIO)
import Data.Char           (toLower)

import System.Hardware.Arduino
import System.Hardware.Arduino.Parts.Servo
import System.Random

--Importing Pilha module to resolve the mean problems 
--import Pilha

--function that associate de servo motor with its port on arduino
--"/dev/cu.usbmodem1421" is the port that my arduino is running it needs to
--be changed according to each computer
servo :: Int -> IO ()
servo a = withArduino False "/dev/cu.usbmodem1421" $ do
            s <- attach (digital 9) (Just 600) (Just 2400)
            port s
 where port s =  setAngle s a

--Function that make the green LED blink when the user try a wrong answer
blinkRight :: IO ()
blinkRight = withArduino False "/dev/cu.usbmodem1421" $ do
           let led = digital 7
           setPinMode led OUTPUT
           do 	digitalWrite led True
                delay 1000
                digitalWrite led False
                delay 1000

--Function that make the red LED blink when the user try a wrong answer
blinkWrong :: IO ()
blinkWrong = withArduino False "/dev/cu.usbmodem1421" $ do
           let led = digital 8
           setPinMode led OUTPUT
           do 	digitalWrite led True
                delay 1000
                digitalWrite led False
                delay 1000

--Function that move the motor according to its position associating with the interface created
moveServo :: Int -> Int
moveServo a = [178,159, 139, 120, 100, 80, 65, 46, 30, 8, 20, 37, 55, 74, 91, 112, 130, 150, 169] !! a

--Function that run what happen when the user gets a right answer 
--the green LED is blinked and the servo moves 1 point
rightAnswer = do
              blinkRight
              servo (moveServo 1)

--Function that run what happen when the user gets a wrong answer 
--the red LED is blinked and the servo moves to 180 which is the start point
--of the counter
wrongAnswer = do  
              blinkWrong
              servo 180

--Funcionts bellow are going to be used if a predefined list of means are set and
  --the user has to solve them.

--Function taht creates a list of means and get an element of it
--This list is composed by maps that have the mean to be solved and the answer of it
--getMean :: Int -> Map
--getMean x = [<"2,4,6","4">, <"2,4,10,8","6">, <"2,4", "3">] :: x

--Function that makes an random number to access a list of mean problems
--getRandom

-------------------------
-- The functions below are going to be used if on user enter the mean to be solved
  --and the other one answer it. (Instead of a list of means)
--Function that gets a mean specified for the first user
--getMean :: IO() -> Pilha
--getMean = do
 --           putStrLn "Jogo o Nésimo numero da média a ser resolvido"
 --           meanNumber <- getLine
 --          empilha (Pilha p) (meanNumber)

--Function that resolves the mean
--resolveMean :: Pilha -> Float
--resolveMean = PilhaVazia
--resolveMean = retornaTopo p + resolveMean (desempilha p) / "contador"
---------------------------

main =

    forever $ do  
        putStrLn "Jogo da média"
        putStrLn ""
        operacao <- getLine
                  --let medias = []
                  --do     
                  --  putStrLn "Entre os elementos da media um a um"
                  --  a <-getLine
                   -- a : medias
                   -- while (a /= 0)
        case operacao of
       	 -- Soma
          "1" -> do
          
    			  	putStrLn "Resposta Correta"
       				blinkRight
              --servo (moveServo +1)
       				servo (moveServo 1)


         -- Erro
          _   -> do 	
         			putStrLn "Resposta Errada! "
         			blinkWrong
         			servo 180

