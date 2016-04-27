{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivory.OS.Posix.Tower.Lib.WiringPi where


import Ivory.Language
import Ivory.Language.Type

wiringPi_header :: String
wiringPi_header = "wiringPi.h"

softTone_header :: String
softTone_header = "softTone.h"

-------------
-- TYPES FOR 32 BITS
-------------

type CSize_t = Uint32
type CInt    = Sint32


------------------
--SETUP FUNCTIONS
-- see http://wiringpi.com/reference/setup/
-------------------

-- Setup with the wiringPi numbering
wiringPiSetup :: Def ('[] ':-> CInt)
wiringPiSetup = importProc "wiringPiSetup" wiringPi_header

-- Setup the interface with /sys/class/gpio instead of accessing hardware. Note, you can access only pins that have been exported first.
wiringPiSetupSys :: Def ('[] ':-> CInt)
wiringPiSetupSys = importProc "wiringPiSetupSys" wiringPi_header

-- Setup with the Broadcom gpio numbering
wiringPiSetupGpio :: Def ('[] ':-> CInt)
wiringPiSetupGpio = importProc "wiringPiSetupGpio" wiringPi_header

-- Same as setup gpio, but allows access only to P1 connector (http://elinux.org/RPi_Hardware)
wiringPiSetupPhys :: Def ('[] ':-> CInt)
wiringPiSetupPhys = importProc "wiringPiSetupPhys" wiringPi_header

-----------------------
-- CORE FUNCTIONS --
-- see http://wiringpi.com/reference/core-functions/
-----------------------

wp_INPUT, wp_OUTPUT, wp_PWM_OUTPUT, wp_GPIO_CLOCK :: CInt
wp_INPUT = extern "INPUT" wiringPi_header
wp_OUTPUT = extern "OUTPUT" wiringPi_header
wp_PWM_OUTPUT = extern "PWM_OUTPUT" wiringPi_header
wp_GPIO_CLOCK = extern "GPIO_CLOCK" wiringPi_header

wp_PUD_OFF, wp_PUD_DOWN, wp_PUD_UP :: CInt
wp_PUD_OFF = extern "PUD_OFF" wiringPi_header
wp_PUD_DOWN = extern "PUD_DOWN" wiringPi_header
wp_PUD_UP = extern "PUD_UP" wiringPi_header

wp_HIGH, wp_LOW :: CInt
wp_HIGH = extern "HIGH" wiringPi_header
wp_LOW = extern "LOW" wiringPi_header

--pinMode version that can set any pin to any mode /!\ UNSAFE
--void pinModeAlt (int pin, int mode)
pinModeAlt :: Def ('[CInt, CInt] ':-> ())
pinModeAlt = importProc "pinModeAlt" wiringPi_header

-- This sets the mode of a pin to either INPUT, OUTPUT, PWM_OUTPUT or GPIO_CLOCK. 
-- Note that only wiringPi pin 1 (BCM_GPIO 18) supports PWM output and only 
-- wiringPi pin 7 (BCM_GPIO 4) supports CLOCK output modes.
-- void pinMode (int pin, int mode)
pinMode :: Def ('[CInt, CInt] ':-> ())
pinMode = importProc "pinMode" wiringPi_header

-- This sets the pull-up or pull-down resistor mode on the given pin, which should be set as an input. 
-- Unlike the Arduino, the BCM2835 has both pull-up an down internal resistors. 
-- The parameter pud should be; PUD_OFF, (no pull up/down), PUD_DOWN (pull to ground) or PUD_UP (pull to 3.3v) 
-- The internal pull up/down resistors have a value of approximately 50KΩ on the Raspberry Pi.
-- void pullUpDnControl (int pin, int pud)
pullUpDnControl :: Def ('[CInt, CInt] ':-> ())
pullUpDnControl = importProc "pullUpDnControl" wiringPi_header

-- This function returns the value read at the given pin. 
-- It will be HIGH or LOW (1 or 0) depending on the logic level at the pin.
digitalRead :: Def ('[CInt] ':-> CInt)
digitalRead = importProc "digitalRead" wiringPi_header

-- Writes the value HIGH or LOW (1 or 0) to the 
-- given pin which must have been previously set as an output.
-- void digitalWrite (int pin, int value)
digitalWrite :: Def ('[CInt, CInt] ':-> ())
digitalWrite = importProc "digitalWrite" wiringPi_header

-- Writes the value to the PWM register for the given pin. 
-- The Raspberry Pi has one on-board PWM pin, pin 1 (BMC_GPIO 18, Phys 12) 
-- and the range is 0-1024. Other PWM devices may have other PWM ranges.
pwmWrite :: Def ('[CInt, CInt] ':-> ())
pwmWrite = importProc "pwmWrite" wiringPi_header

-- This returns the value read on the supplied analog input pin. 
-- You will need to register additional analog modules to enable this 
-- function for devices such as the Gertboard, quick2Wire analog board, etc.
analogRead :: Def ('[CInt] ':-> CInt)
analogRead = importProc "analogRead" wiringPi_header

-- This writes the given value to the supplied analog pin. 
-- You will need to register additional analog modules to enable this 
-- function for devices such as the Gertboard.
analogWrite :: Def ('[CInt, CInt] ':-> ())
analogWrite = importProc "analogWrite" wiringPi_header


------------------
-- SOFT TONE LIBRARY
------------------

-- This creates a software controlled tone pin. You can use any GPIO pin and the pin 
-- numbering will be that of the wiringPiSetup() function you used.
-- The return value is 0 for success. Anything else and you should 
-- check the global errno variable to see what went wrong.
softToneCreate :: Def ('[CInt] ':-> CInt)
softToneCreate = importProc "softToneCreate" softTone_header

-- This updates the tone frequency value on the given pin. 
-- The tone will be played until you set the frequency to 0.
softToneWrite :: Def ('[CInt, CInt] ':-> ())
softToneWrite = importProc "softToneWrite" softTone_header


uses_wiringPi :: ModuleDef
uses_wiringPi = do
  inclSym wp_INPUT
  inclSym wp_OUTPUT
  inclSym wp_PWM_OUTPUT
  inclSym wp_GPIO_CLOCK

  inclSym wp_PUD_OFF
  inclSym wp_PUD_DOWN
  inclSym wp_PUD_UP

  inclSym wp_HIGH
  inclSym wp_LOW
  
  incl wiringPiSetup
  incl wiringPiSetupPhys
  incl wiringPiSetupGpio
  incl wiringPiSetupSys

  incl pinModeAlt
  incl pinMode
  incl pullUpDnControl
  incl digitalRead
  incl digitalWrite
  incl pwmWrite
  incl analogRead
  incl analogWrite

  incl softToneCreate
  incl softToneWrite
