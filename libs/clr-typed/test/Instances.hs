{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}

module Instances where

import Clr
import Clr.Bridge
import Clr.Marshal
import Clr.UnmarshalAs

import Data.Int
import Data.Word
import Foreign.Ptr
import Data.Text

-- Just for testing. Actual use is BStr
type instance BridgeTypePrim T_string = String
type instance UnmarshalAs String = String

-- Synonyms while we have to still write this manually
type T_Console      = T "System.Console" '[]
type T_BaseType     = T "BaseType" '[]
type T_DerivedType  = T "DerivedType" '[]
type T_StringIntDel = T "StringIntDel" '[]

-- Synonyms for methods
type T_WriteLine = T "WriteLine" '[]
type T_Foo       = T "Foo" '[]
type T_Bar       = T "Bar" '[]
type T_Add       = T "Add" '[]

type instance SuperTypes T_BaseType             = '[ T_object ]
type instance SuperTypes T_DerivedType          = '[ T_BaseType ]
type instance SuperTypes (T "MyGenType" '[gt0]) = '[ T_object, T "IEnumerable" '[], T "IEnumerable" '[gt0] ]

type instance Members T_BaseType             = '[T_Foo, T_Bar]
type instance Members T_DerivedType          = '[T_Foo]
type instance Members (T "MyGenType" '[gt0]) = '[T_Add]

type instance Candidates T_Console T_WriteLine = '[ '[T_string], '[T_string, T_string] ]
type instance Candidates T_BaseType T_BaseType = '[ '[] ]
type instance Candidates T_BaseType T_Foo = '[ '[ T_string ], '[ T_long ], '[ T_int ] ]
type instance Candidates T_BaseType T_Bar = '[ '[ T_string ], '[ T_long ], '[ T_int ] ]
type instance Candidates T_DerivedType T_DerivedType = '[ '[] ]
type instance Candidates T_DerivedType T_Foo = '[ '[ T_string ], '[ T_long ], '[ T_int ]]
type instance Candidates (T "MyGenType" '[gt0]) (T "MyGenType" '[gt0]) = '[ '[] ]
type instance Candidates (T "MyGenType" '[gt0]) (T_Add) = '[ '[ gt0 ] ]

writeLineRaw1 :: String -> IO String
writeLineRaw1 cs = return "Console.WriteLine(String)"
writeLineRaw2 :: String -> String -> IO String
writeLineRaw2 cs1 cs2 = return "Console.WriteLine(String,String)"
rawInvokeBaseTypeFooStr :: ObjectID t -> String -> IO String
rawInvokeBaseTypeFooStr d s = return "BaseType.Foo(String)"
rawInvokeBaseTypeFooInt64 :: ObjectID t -> Int64 -> IO String
rawInvokeBaseTypeFooInt64 d s = return "BaseType.Foo(Int64)"
rawInvokeBaseTypeFooInt32 :: ObjectID t -> Int32 -> IO String
rawInvokeBaseTypeFooInt32 d s = return "BaseType.Foo(Int32)"
rawInvokeDerivedTypeStr :: ObjectID t -> String -> IO String
rawInvokeDerivedTypeStr d s = return "DerivedType.Foo(String)"
rawInvokeDerivedTypeInt64 :: ObjectID t -> Int64 -> IO String
rawInvokeDerivedTypeInt64 d s = return "DerivedType.Foo(Int64)"
rawInvokeDerivedTypeInt32 :: ObjectID t -> Int32 -> IO String
rawInvokeDerivedTypeInt32 d s = return "DerivedType.Foo(Int32)"
rawInvokeBaseTypeBarStr :: ObjectID t -> String -> IO String
rawInvokeBaseTypeBarStr d s = return "BaseType.Bar(String)"
rawInvokeBaseTypeBarInt64 :: ObjectID t -> Int64 -> IO String
rawInvokeBaseTypeBarInt64 d s = return "BaseType.Bar(Int64)"
rawInvokeBaseTypeBarInt32 :: ObjectID t -> Int32 -> IO String
rawInvokeBaseTypeBarInt32 d s = return "BaseType.Bar(Int32)"
rawInvokeMyGenTypeAddStr :: ObjectID t -> String -> IO String
rawInvokeMyGenTypeAddStr oid s = return "MyGenType.Add(String)"
rawInvokeMyGenTypeAddInt :: ObjectID t -> Int32 -> IO String
rawInvokeMyGenTypeAddInt oid s = return "MyGenType.Add(Int32)"


instance MethodResultS1 T_Console T_WriteLine T_string where
  type ResultTypeS1 T_Console T_WriteLine T_string = 'Just T_string

instance MethodInvokeS1 T_Console T_WriteLine T_string where
  rawInvokeS1 = writeLineRaw1

instance MethodResultS2 T_Console T_WriteLine T_string T_string where
  type ResultTypeS2 T_Console T_WriteLine T_string T_string = 'Just T_string

instance MethodInvokeS2 T_Console T_WriteLine T_string T_string where
  rawInvokeS2 = writeLineRaw2

instance Constructor1 T_BaseType () where
  rawNew1 () = return (ObjectID 1)

instance MethodResultI1 T_BaseType T_Foo T_string where
  type ResultTypeI1 T_BaseType T_Foo T_string = 'Just T_string

instance MethodInvokeI1 T_BaseType T_Foo T_string where
  rawInvokeI1 = rawInvokeBaseTypeFooStr

instance MethodResultI1 T_BaseType T_Foo T_long where
  type ResultTypeI1 T_BaseType T_Foo T_long = 'Just T_string

instance MethodInvokeI1 T_BaseType T_Foo T_long where
  rawInvokeI1 = rawInvokeBaseTypeFooInt64

instance MethodResultI1 T_BaseType T_Foo T_int where
  type ResultTypeI1 T_BaseType T_Foo T_int = 'Just T_string

instance MethodInvokeI1 T_BaseType T_Foo T_int where
  rawInvokeI1 = rawInvokeBaseTypeFooInt32

instance MethodResultI1 T_BaseType T_Bar T_string where
  type ResultTypeI1 T_BaseType T_Bar T_string = 'Just T_string

instance MethodInvokeI1 T_BaseType T_Bar T_string where
  rawInvokeI1 = rawInvokeBaseTypeBarStr

instance MethodResultI1 T_BaseType T_Bar T_long where
  type ResultTypeI1 T_BaseType T_Bar T_long = 'Just T_string

instance MethodInvokeI1 T_BaseType T_Bar T_long where
  rawInvokeI1 = rawInvokeBaseTypeBarInt64

instance MethodResultI1 T_BaseType T_Bar T_int where
  type ResultTypeI1 T_BaseType T_Bar T_int = 'Just T_string

instance MethodInvokeI1 T_BaseType T_Bar T_int where
  rawInvokeI1 = rawInvokeBaseTypeBarInt32

instance Constructor1 T_DerivedType () where
  rawNew1 () = return (ObjectID 1)

instance MethodResultI1 T_DerivedType T_Foo T_string where
  type ResultTypeI1 T_DerivedType T_Foo T_string = 'Just T_string

instance MethodInvokeI1 T_DerivedType T_Foo T_string where
  rawInvokeI1 = rawInvokeDerivedTypeStr

instance MethodResultI1 T_DerivedType T_Foo T_long where
  type ResultTypeI1 T_DerivedType T_Foo T_long = 'Just T_string

instance MethodInvokeI1 T_DerivedType T_Foo T_long where
  rawInvokeI1 = rawInvokeDerivedTypeInt64

instance MethodResultI1 T_DerivedType T_Foo T_int where
  type ResultTypeI1 T_DerivedType T_Foo T_int = 'Just T_string

instance MethodInvokeI1 T_DerivedType T_Foo T_int where
  rawInvokeI1 = rawInvokeDerivedTypeInt32

instance Constructor1 (T "MyGenType" '[gt0]) () where
  rawNew1 () = return (ObjectID 1)

instance MethodResultI1 (T "MyGenType" '[T_string]) T_Add T_string where
  type ResultTypeI1 (T "MyGenType" '[T_string]) T_Add T_string = 'Just T_string

instance MethodInvokeI1 (T "MyGenType" '[T_string]) T_Add T_string where
  rawInvokeI1 = rawInvokeMyGenTypeAddStr

instance MethodResultI1 (T "MyGenType" '[T_int]) T_Add T_int where
  type ResultTypeI1 (T "MyGenType" '[T_int]) T_Add T_int = 'Just T_string

instance MethodInvokeI1 (T "MyGenType" '[T_int]) T_Add T_int where
  rawInvokeI1 = rawInvokeMyGenTypeAddInt

instance Delegate T_StringIntDel where
  type DelegateArgTypes   T_StringIntDel = '[ T_string ]
  type DelegateResultType T_StringIntDel = 'Just T_int

instance DelegateConstructor1 T_StringIntDel where
  rawConstructDelegate1 f = return (ObjectID 1 :: ObjectID T_StringIntDel)

instance Marshal String Text where
  marshal s f = f $ pack s
