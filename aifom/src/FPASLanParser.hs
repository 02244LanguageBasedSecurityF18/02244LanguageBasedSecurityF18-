{-# OPTIONS_GHC -w #-}
{-

Open Source Fixedpoint Model-Checker version 2010

(C) Copyright IBM Corp. 2010

All Rights Reserved.

Modifications by Paolo Modesti 2010

-}

module FPASLanParser where
import Data.Char
import Data.List
import FPASLanLexer
import Types
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t5
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (PASLan)
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 ([PTerm])
	| HappyAbsSyn7 (([(PIdent,Int)], [(PIdent,Int)]))
	| HappyAbsSyn8 (PDefinitions)
	| HappyAbsSyn10 ([Typedec])
	| HappyAbsSyn11 ([PIdent])
	| HappyAbsSyn13 ([Vardec])
	| HappyAbsSyn16 (PType)
	| HappyAbsSyn20 (PTerm)
	| HappyAbsSyn21 ([(PIdent,Int)])
	| HappyAbsSyn22 ([PRule])
	| HappyAbsSyn23 (PRule)
	| HappyAbsSyn24 ((PIdent, [Vardec]))
	| HappyAbsSyn25 (([PFact],[PCond],[PCond]))
	| HappyAbsSyn29 (PFact)

action_0 (33) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (33) = happyShift action_2
action_1 _ = happyFail

action_2 (31) = happyShift action_4
action_2 _ = happyFail

action_3 (66) = happyAccept
action_3 _ = happyFail

action_4 (48) = happyShift action_5
action_4 _ = happyFail

action_5 (34) = happyShift action_7
action_5 (5) = happyGoto action_6
action_5 _ = happyReduce_3

action_6 (35) = happyShift action_12
action_6 (6) = happyGoto action_11
action_6 _ = happyReduce_5

action_7 (31) = happyShift action_10
action_7 (10) = happyGoto action_8
action_7 (12) = happyGoto action_9
action_7 _ = happyReduce_16

action_8 _ = happyReduce_2

action_9 (49) = happyShift action_22
action_9 _ = happyFail

action_10 (50) = happyShift action_20
action_10 (65) = happyShift action_21
action_10 _ = happyReduce_19

action_11 (36) = happyShift action_19
action_11 (7) = happyGoto action_18
action_11 _ = happyFail

action_12 (31) = happyShift action_15
action_12 (53) = happyShift action_16
action_12 (61) = happyShift action_17
action_12 (19) = happyGoto action_13
action_12 (20) = happyGoto action_14
action_12 _ = happyFail

action_13 (48) = happyShift action_43
action_13 _ = happyFail

action_14 (50) = happyShift action_42
action_14 _ = happyReduce_35

action_15 (53) = happyShift action_40
action_15 (55) = happyShift action_41
action_15 _ = happyReduce_37

action_16 (31) = happyShift action_39
action_16 _ = happyFail

action_17 _ = happyReduce_38

action_18 (39) = happyShift action_38
action_18 _ = happyFail

action_19 (31) = happyShift action_36
action_19 (37) = happyShift action_37
action_19 (21) = happyGoto action_35
action_19 _ = happyFail

action_20 (31) = happyShift action_34
action_20 (12) = happyGoto action_33
action_20 _ = happyFail

action_21 (31) = happyShift action_31
action_21 (51) = happyShift action_32
action_21 (15) = happyGoto action_29
action_21 (18) = happyGoto action_30
action_21 _ = happyFail

action_22 (31) = happyShift action_25
action_22 (42) = happyShift action_26
action_22 (43) = happyShift action_27
action_22 (51) = happyShift action_28
action_22 (16) = happyGoto action_23
action_22 (17) = happyGoto action_24
action_22 _ = happyFail

action_23 (48) = happyShift action_58
action_23 _ = happyFail

action_24 _ = happyReduce_28

action_25 _ = happyReduce_29

action_26 _ = happyReduce_30

action_27 _ = happyReduce_31

action_28 (31) = happyShift action_53
action_28 (11) = happyGoto action_57
action_28 _ = happyFail

action_29 _ = happyReduce_34

action_30 (48) = happyShift action_56
action_30 _ = happyFail

action_31 (59) = happyShift action_55
action_31 _ = happyReduce_25

action_32 (31) = happyShift action_53
action_32 (60) = happyShift action_54
action_32 (11) = happyGoto action_52
action_32 _ = happyFail

action_33 _ = happyReduce_20

action_34 (50) = happyShift action_20
action_34 _ = happyReduce_19

action_35 (48) = happyShift action_51
action_35 _ = happyFail

action_36 (57) = happyShift action_50
action_36 _ = happyFail

action_37 (31) = happyShift action_36
action_37 (21) = happyGoto action_49
action_37 _ = happyFail

action_38 (31) = happyShift action_36
action_38 (21) = happyGoto action_48
action_38 _ = happyFail

action_39 (54) = happyShift action_47
action_39 _ = happyFail

action_40 (31) = happyShift action_15
action_40 (53) = happyShift action_16
action_40 (61) = happyShift action_17
action_40 (19) = happyGoto action_46
action_40 (20) = happyGoto action_14
action_40 _ = happyFail

action_41 (31) = happyShift action_15
action_41 (53) = happyShift action_16
action_41 (61) = happyShift action_17
action_41 (19) = happyGoto action_45
action_41 (20) = happyGoto action_14
action_41 _ = happyFail

action_42 (31) = happyShift action_15
action_42 (53) = happyShift action_16
action_42 (61) = happyShift action_17
action_42 (19) = happyGoto action_44
action_42 (20) = happyGoto action_14
action_42 _ = happyFail

action_43 _ = happyReduce_4

action_44 _ = happyReduce_36

action_45 (56) = happyShift action_70
action_45 _ = happyFail

action_46 (54) = happyShift action_69
action_46 _ = happyFail

action_47 _ = happyReduce_41

action_48 (48) = happyShift action_68
action_48 _ = happyFail

action_49 (48) = happyShift action_67
action_49 _ = happyFail

action_50 (32) = happyShift action_66
action_50 _ = happyFail

action_51 _ = happyReduce_7

action_52 (52) = happyShift action_65
action_52 _ = happyFail

action_53 (50) = happyShift action_64
action_53 _ = happyReduce_17

action_54 (60) = happyShift action_63
action_54 _ = happyFail

action_55 (31) = happyShift action_31
action_55 (15) = happyGoto action_62
action_55 _ = happyFail

action_56 (31) = happyShift action_10
action_56 (10) = happyGoto action_61
action_56 (12) = happyGoto action_9
action_56 _ = happyReduce_16

action_57 (52) = happyShift action_60
action_57 _ = happyFail

action_58 (31) = happyShift action_10
action_58 (10) = happyGoto action_59
action_58 (12) = happyGoto action_9
action_58 _ = happyReduce_16

action_59 _ = happyReduce_14

action_60 _ = happyReduce_27

action_61 _ = happyReduce_15

action_62 _ = happyReduce_26

action_63 (60) = happyShift action_76
action_63 _ = happyFail

action_64 (31) = happyShift action_53
action_64 (11) = happyGoto action_75
action_64 _ = happyFail

action_65 _ = happyReduce_32

action_66 (50) = happyShift action_74
action_66 _ = happyReduce_42

action_67 (38) = happyShift action_73
action_67 _ = happyFail

action_68 (41) = happyShift action_72
action_68 (8) = happyGoto action_71
action_68 _ = happyReduce_8

action_69 _ = happyReduce_40

action_70 _ = happyReduce_39

action_71 (40) = happyShift action_82
action_71 _ = happyFail

action_72 (31) = happyShift action_81
action_72 (9) = happyGoto action_80
action_72 _ = happyFail

action_73 (31) = happyShift action_36
action_73 (21) = happyGoto action_79
action_73 _ = happyFail

action_74 (31) = happyShift action_36
action_74 (21) = happyGoto action_78
action_74 _ = happyFail

action_75 _ = happyReduce_18

action_76 (52) = happyShift action_77
action_76 _ = happyFail

action_77 _ = happyReduce_33

action_78 _ = happyReduce_43

action_79 (48) = happyShift action_90
action_79 _ = happyFail

action_80 _ = happyReduce_9

action_81 (49) = happyShift action_88
action_81 (53) = happyShift action_89
action_81 _ = happyFail

action_82 (31) = happyShift action_86
action_82 (58) = happyShift action_87
action_82 (66) = happyReduce_45
action_82 (22) = happyGoto action_83
action_82 (23) = happyGoto action_84
action_82 (24) = happyGoto action_85
action_82 _ = happyReduce_49

action_83 _ = happyReduce_1

action_84 (31) = happyShift action_86
action_84 (58) = happyShift action_87
action_84 (66) = happyReduce_45
action_84 (22) = happyGoto action_102
action_84 (23) = happyGoto action_84
action_84 (24) = happyGoto action_85
action_84 _ = happyReduce_49

action_85 (31) = happyShift action_100
action_85 (46) = happyShift action_101
action_85 (25) = happyGoto action_96
action_85 (27) = happyGoto action_97
action_85 (28) = happyGoto action_98
action_85 (29) = happyGoto action_99
action_85 _ = happyReduce_51

action_86 (55) = happyShift action_95
action_86 _ = happyFail

action_87 (31) = happyShift action_94
action_87 (14) = happyGoto action_93
action_87 _ = happyFail

action_88 (31) = happyShift action_15
action_88 (53) = happyShift action_16
action_88 (61) = happyShift action_17
action_88 (20) = happyGoto action_92
action_88 _ = happyFail

action_89 (31) = happyShift action_34
action_89 (12) = happyGoto action_91
action_89 _ = happyFail

action_90 _ = happyReduce_6

action_91 (54) = happyShift action_116
action_91 _ = happyFail

action_92 (48) = happyShift action_115
action_92 _ = happyReduce_10

action_93 (60) = happyShift action_114
action_93 _ = happyFail

action_94 (50) = happyShift action_113
action_94 _ = happyReduce_23

action_95 (31) = happyShift action_112
action_95 (13) = happyGoto action_111
action_95 _ = happyFail

action_96 (62) = happyShift action_109
action_96 (63) = happyShift action_110
action_96 (30) = happyGoto action_108
action_96 _ = happyFail

action_97 (60) = happyShift action_107
action_97 _ = happyReduce_52

action_98 _ = happyReduce_56

action_99 _ = happyReduce_59

action_100 (44) = happyShift action_104
action_100 (45) = happyShift action_105
action_100 (55) = happyShift action_106
action_100 _ = happyReduce_62

action_101 (31) = happyShift action_34
action_101 (12) = happyGoto action_103
action_101 _ = happyFail

action_102 _ = happyReduce_44

action_103 (60) = happyShift action_130
action_103 _ = happyFail

action_104 (31) = happyShift action_15
action_104 (53) = happyShift action_16
action_104 (61) = happyShift action_17
action_104 (20) = happyGoto action_129
action_104 _ = happyFail

action_105 (31) = happyShift action_15
action_105 (53) = happyShift action_16
action_105 (61) = happyShift action_17
action_105 (20) = happyGoto action_128
action_105 _ = happyFail

action_106 (31) = happyShift action_15
action_106 (53) = happyShift action_16
action_106 (61) = happyShift action_17
action_106 (19) = happyGoto action_127
action_106 (20) = happyGoto action_14
action_106 _ = happyFail

action_107 (31) = happyShift action_100
action_107 (46) = happyShift action_101
action_107 (25) = happyGoto action_126
action_107 (27) = happyGoto action_97
action_107 (28) = happyGoto action_98
action_107 (29) = happyGoto action_99
action_107 _ = happyReduce_51

action_108 (31) = happyShift action_125
action_108 (26) = happyGoto action_123
action_108 (28) = happyGoto action_124
action_108 (29) = happyGoto action_99
action_108 _ = happyReduce_54

action_109 _ = happyReduce_63

action_110 (31) = happyShift action_34
action_110 (12) = happyGoto action_122
action_110 _ = happyFail

action_111 (56) = happyShift action_121
action_111 _ = happyFail

action_112 (49) = happyShift action_120
action_112 _ = happyFail

action_113 (31) = happyShift action_94
action_113 (14) = happyGoto action_119
action_113 _ = happyFail

action_114 _ = happyReduce_47

action_115 (31) = happyShift action_81
action_115 (9) = happyGoto action_118
action_115 _ = happyFail

action_116 (49) = happyShift action_117
action_116 _ = happyFail

action_117 (31) = happyShift action_15
action_117 (53) = happyShift action_16
action_117 (61) = happyShift action_17
action_117 (20) = happyGoto action_137
action_117 _ = happyFail

action_118 _ = happyReduce_12

action_119 _ = happyReduce_24

action_120 (31) = happyShift action_25
action_120 (42) = happyShift action_26
action_120 (43) = happyShift action_27
action_120 (17) = happyGoto action_136
action_120 _ = happyFail

action_121 _ = happyReduce_48

action_122 (64) = happyShift action_135
action_122 _ = happyFail

action_123 (48) = happyShift action_134
action_123 _ = happyFail

action_124 (60) = happyShift action_133
action_124 _ = happyReduce_55

action_125 (45) = happyShift action_105
action_125 (55) = happyShift action_106
action_125 _ = happyReduce_62

action_126 _ = happyReduce_50

action_127 (56) = happyShift action_132
action_127 _ = happyFail

action_128 _ = happyReduce_60

action_129 _ = happyReduce_57

action_130 (31) = happyShift action_131
action_130 _ = happyFail

action_131 (44) = happyShift action_141
action_131 _ = happyFail

action_132 _ = happyReduce_61

action_133 (31) = happyShift action_125
action_133 (26) = happyGoto action_140
action_133 (28) = happyGoto action_124
action_133 (29) = happyGoto action_99
action_133 _ = happyReduce_54

action_134 _ = happyReduce_46

action_135 _ = happyReduce_64

action_136 (50) = happyShift action_139
action_136 _ = happyReduce_21

action_137 (48) = happyShift action_138
action_137 _ = happyReduce_11

action_138 (31) = happyShift action_81
action_138 (9) = happyGoto action_144
action_138 _ = happyFail

action_139 (31) = happyShift action_112
action_139 (13) = happyGoto action_143
action_139 _ = happyFail

action_140 _ = happyReduce_53

action_141 (31) = happyShift action_15
action_141 (53) = happyShift action_16
action_141 (61) = happyShift action_17
action_141 (20) = happyGoto action_142
action_141 _ = happyFail

action_142 _ = happyReduce_58

action_143 _ = happyReduce_22

action_144 _ = happyReduce_13

happyReduce_1 = happyReduce 12 4 happyReduction_1
happyReduction_1 ((HappyAbsSyn22  happy_var_12) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn8  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_6) `HappyStk`
	(HappyAbsSyn6  happy_var_5) `HappyStk`
	(HappyAbsSyn5  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (let (pubfn, privfn) = happy_var_6 in (PASLan happy_var_2 happy_var_4 happy_var_5 pubfn privfn happy_var_8 happy_var_10 happy_var_12)
	) `HappyStk` happyRest

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_0  5 happyReduction_3
happyReduction_3  =  HappyAbsSyn5
		 ([]
	)

happyReduce_4 = happySpecReduce_3  6 happyReduction_4
happyReduction_4 _
	(HappyAbsSyn6  happy_var_2)
	_
	 =  HappyAbsSyn6
		 (map atomToComp happy_var_2
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_0  6 happyReduction_5
happyReduction_5  =  HappyAbsSyn6
		 ([]
	)

happyReduce_6 = happyReduce 7 7 happyReduction_6
happyReduction_6 (_ `HappyStk`
	(HappyAbsSyn21  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn21  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 ((happy_var_3, happy_var_6)
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_3  7 happyReduction_7
happyReduction_7 _
	(HappyAbsSyn21  happy_var_2)
	_
	 =  HappyAbsSyn7
		 ((happy_var_2, [])
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_0  8 happyReduction_8
happyReduction_8  =  HappyAbsSyn8
		 ([]
	)

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  9 happyReduction_10
happyReduction_10 (HappyAbsSyn20  happy_var_3)
	_
	(HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn8
		 ([(happy_var_1,happy_var_3,[])]
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happyReduce 6 9 happyReduction_11
happyReduction_11 ((HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 ([(happy_var_1,happy_var_6,happy_var_3)]
	) `HappyStk` happyRest

happyReduce_12 = happyReduce 5 9 happyReduction_12
happyReduction_12 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (((happy_var_1,happy_var_3,[]):happy_var_5)
	) `HappyStk` happyRest

happyReduce_13 = happyReduce 8 9 happyReduction_13
happyReduction_13 ((HappyAbsSyn8  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (((happy_var_1,happy_var_6,happy_var_3):happy_var_8)
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 5 10 happyReduction_14
happyReduction_14 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (([(x,happy_var_3) | x <- happy_var_1] ++ happy_var_5)
	) `HappyStk` happyRest

happyReduce_15 = happyReduce 5 10 happyReduction_15
happyReduction_15 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (((happy_var_1,happy_var_3):happy_var_5)
	) `HappyStk` happyRest

happyReduce_16 = happySpecReduce_0  10 happyReduction_16
happyReduction_16  =  HappyAbsSyn10
		 ([]
	)

happyReduce_17 = happySpecReduce_1  11 happyReduction_17
happyReduction_17 (HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  11 happyReduction_18
happyReduction_18 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn11
		 (happy_var_1:happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  12 happyReduction_19
happyReduction_19 (HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  12 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn11
		 ((happy_var_1:happy_var_3)
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  13 happyReduction_21
happyReduction_21 (HappyAbsSyn16  happy_var_3)
	_
	(HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn13
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 5 13 happyReduction_22
happyReduction_22 ((HappyAbsSyn13  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn16  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 ((happy_var_1, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_23 = happySpecReduce_1  14 happyReduction_23
happyReduction_23 (HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn13
		 ([(happy_var_1, VarOf "")]
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  14 happyReduction_24
happyReduction_24 (HappyAbsSyn13  happy_var_3)
	_
	(HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn13
		 ((happy_var_1, VarOf "") : happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  15 happyReduction_25
happyReduction_25 (HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  15 happyReduction_26
happyReduction_26 (HappyAbsSyn11  happy_var_3)
	_
	(HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn11
		 ((happy_var_1:happy_var_3)
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  16 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (Enum happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn16
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  17 happyReduction_29
happyReduction_29 (HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn16
		 (VarOf happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn16
		 (Value
	)

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn16
		 (Untyped
	)

happyReduce_32 = happySpecReduce_3  18 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn16
		 (EnumType happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happyReduce 5 18 happyReduction_33
happyReduction_33 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn16
		 (CountType
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn16
		 (Union happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  19 happyReduction_35
happyReduction_35 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn6
		 ([happy_var_1]
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_3  19 happyReduction_36
happyReduction_36 (HappyAbsSyn6  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn6
		 ((happy_var_1:happy_var_3)
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  20 happyReduction_37
happyReduction_37 (HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn20
		 (PAtom happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  20 happyReduction_38
happyReduction_38 _
	 =  HappyAbsSyn20
		 (PAtom "_"
	)

happyReduce_39 = happyReduce 4 20 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (PComp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 4 20 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (PPar happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_3  20 happyReduction_41
happyReduction_41 _
	(HappyTerminal (Tident _ happy_var_2))
	_
	 =  HappyAbsSyn20
		 (PParId happy_var_2
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  21 happyReduction_42
happyReduction_42 (HappyTerminal (Tint _ happy_var_3))
	_
	(HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn21
		 ([(happy_var_1,happy_var_3)]
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happyReduce 5 21 happyReduction_43
happyReduction_43 ((HappyAbsSyn21  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tint _ happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn21
		 ((happy_var_1,happy_var_3):happy_var_5
	) `HappyStk` happyRest

happyReduce_44 = happySpecReduce_2  22 happyReduction_44
happyReduction_44 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn22
		 (happy_var_1:happy_var_2
	)
happyReduction_44 _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_0  22 happyReduction_45
happyReduction_45  =  HappyAbsSyn22
		 ([]
	)

happyReduce_46 = happyReduce 5 23 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyAbsSyn25  happy_var_4) `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	(HappyAbsSyn25  happy_var_2) `HappyStk`
	(HappyAbsSyn24  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn23
		 ((\ (nm,env) (lf,lp,ln) (rf,rp,_) ->
      PRule { ruleName = nm, ruleEnv = env,
              lf = lf, lp = lp, ln = ln,
              fresh = happy_var_3, rf = rf, rp = rp }) happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_3  24 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn13  happy_var_2)
	_
	 =  HappyAbsSyn24
		 (("lambda", happy_var_2)
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 4 24 happyReduction_48
happyReduction_48 (_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn24
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_0  24 happyReduction_49
happyReduction_49  =  HappyAbsSyn24
		 (("lambda", [])
	)

happyReduce_50 = happySpecReduce_3  25 happyReduction_50
happyReduction_50 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (combside happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_0  25 happyReduction_51
happyReduction_51  =  HappyAbsSyn25
		 (([],[],[])
	)

happyReduce_52 = happySpecReduce_1  25 happyReduction_52
happyReduction_52 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  26 happyReduction_53
happyReduction_53 (HappyAbsSyn25  happy_var_3)
	_
	(HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (combside happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_0  26 happyReduction_54
happyReduction_54  =  HappyAbsSyn25
		 (([],[],[])
	)

happyReduce_55 = happySpecReduce_1  26 happyReduction_55
happyReduction_55 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  27 happyReduction_56
happyReduction_56 (HappyAbsSyn25  happy_var_1)
	 =  HappyAbsSyn25
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  27 happyReduction_57
happyReduction_57 (HappyAbsSyn20  happy_var_3)
	_
	(HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn25
		 ([],[],[(PCond [] happy_var_1 (atomToComp happy_var_3))]
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happyReduce 6 27 happyReduction_58
happyReduction_58 ((HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn25
		 ([],[],[(PCond happy_var_2 happy_var_4 (atomToComp happy_var_6))]
	) `HappyStk` happyRest

happyReduce_59 = happySpecReduce_1  28 happyReduction_59
happyReduction_59 (HappyAbsSyn29  happy_var_1)
	 =  HappyAbsSyn25
		 (([happy_var_1],[],[])
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_3  28 happyReduction_60
happyReduction_60 (HappyAbsSyn20  happy_var_3)
	_
	(HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn25
		 ([],[(PCond [] happy_var_1 (atomToComp happy_var_3))],[]
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 4 29 happyReduction_61
happyReduction_61 (_ `HappyStk`
	(HappyAbsSyn6  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (Tident _ happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn29
		 ((PFact happy_var_1 happy_var_3)
	) `HappyStk` happyRest

happyReduce_62 = happySpecReduce_1  29 happyReduction_62
happyReduction_62 (HappyTerminal (Tident _ happy_var_1))
	 =  HappyAbsSyn29
		 ((PFact happy_var_1 [])
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  30 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn11
		 ([]
	)

happyReduce_64 = happySpecReduce_3  30 happyReduction_64
happyReduction_64 _
	(HappyAbsSyn11  happy_var_2)
	_
	 =  HappyAbsSyn11
		 (happy_var_2
	)
happyReduction_64 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 66 66 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	Tident _ happy_dollar_dollar -> cont 31;
	Tint _ happy_dollar_dollar -> cont 32;
	Tproblem _ -> cont 33;
	Ttypes _ -> cont 34;
	Tsets _ -> cont 35;
	Tfunctions _ -> cont 36;
	Tpublic _ -> cont 37;
	Tprivate _ -> cont 38;
	TFacts _ -> cont 39;
	TRules _ -> cont 40;
	TDefinitions _ -> cont 41;
	Tvalue _ -> cont 42;
	Tuntyped _ -> cont 43;
	Tnotin _ -> cont 44;
	Tin _ -> cont 45;
	Tforall _ -> cont 46;
	Texists _ -> cont 47;
	Tsemicolon _ -> cont 48;
	Tcolon _ -> cont 49;
	Tcomma _ -> cont 50;
	Topenbrace _ -> cont 51;
	Tclosebrace _ -> cont 52;
	Topensquare _ -> cont 53;
	Tclosesquare _ -> cont 54;
	Topenpar _ -> cont 55;
	Tclosepar _ -> cont 56;
	Tslash _ -> cont 57;
	Tlambda _ -> cont 58;
	Tunion _ -> cont 59;
	Tdot _ -> cont 60;
	Twild _ -> cont 61;
	Tarrow _ -> cont 62;
	Topenarrow _ -> cont 63;
	Tclosarrow _ -> cont 64;
	Tequal _ -> cont 65;
	_ -> happyError' (tk:tks)
	}

happyError_ 66 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = return
    (<*>) = ap
instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parser tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n" )
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c ++ " - Token: " ++ show tk
			where
			AlexPn _ l c = token_posn tk
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
