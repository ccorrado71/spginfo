MODULE symm_table

implicit none

integer, parameter :: LEN_HM = 20

type sg_info_type
     integer               :: num    = 0    ! numero
     character(len=5)      :: choice = ' '  ! table choice
     character(len=LEN_HM) :: hm1    = ' '  ! H-M symbol
     character(len=11)     :: hm2    = ' '  ! H-M symbol
     character(len=17)     :: hall   = ' '  ! Hall symbol
     character(len=8)      :: pmat   = ' '  ! Transformation matrix to standard setting
end type sg_info_type

type extsymb_t
     character(len=20)      :: symb = ' '   ! symbol of extinction group
     integer                :: nspg  = 0    ! number of compatible space groups
     integer, dimension(16) :: spgn  = 0    ! order numbers of compatible space groups in array sg_info
end type extsymb_t
integer, parameter :: SG_NUMBER = 530

type(sg_info_type), dimension(SG_NUMBER) :: sg_info = (/ &
 sg_info_type(  1,'     ','P1                  ','P 1        ',' P 1             ','a,b,c   '),       &
 sg_info_type(  2,'     ','P-1                 ','P -1       ','-P 1             ','a,b,c   '),       &
 sg_info_type(  3,'b    ','P2:b = P121         ','P 2        ',' P 2y            ','a,b,c   '),       &
 sg_info_type(  3,'c    ','P2:c = P112         ','P 1 1 2    ',' P 2             ','b,c,a   '),       &
 sg_info_type(  3,'a    ','P2:a = P211         ','P 2 1 1    ',' P 2x            ','c,a,b   '),       &
 sg_info_type(  4,'b    ','P21:b = P1211       ','P 21       ',' P 2yb           ','a,b,c   '),       &
 sg_info_type(  4,'c    ','P21:c = P1121       ','P 1 1 21   ',' P 2c            ','b,c,a   '),       &
 sg_info_type(  4,'a    ','P21:a = P2111       ','P 21 1 1   ',' P 2xa           ','c,a,b   '),       &
 sg_info_type(  5,'b1   ','C2:b1 = C121        ','C 2        ',' C 2y            ','a,b,c   '),       &
 sg_info_type(  5,'b2   ','A2:b2 = A121        ','A 2        ',' A 2y            ','c,b,-a-c'),       &
 sg_info_type(  5,'b3   ','I2:b3 = I121        ','I 2        ',' I 2y            ','-a-c,b,a'),       &
 sg_info_type(  5,'c1   ','A2:c1 = A112        ','A 1 1 2    ',' A 2             ','b,c,a   '),       &
 sg_info_type(  5,'c2   ','B2:c2 = B112        ','B 1 1 2    ',' B 2             ','a,c,-a-b'),       &
 sg_info_type(  5,'c3   ','I2:c3 = I112        ','I 1 1 2    ',' I 2             ','-a-b,c,b'),       &
 sg_info_type(  5,'a1   ','B2:a1 = B211        ','B 2 1 1    ',' B 2x            ','c,a,b   '),       &
 sg_info_type(  5,'a2   ','C2:a2 = C211        ','C 2 1 1    ',' C 2x            ','b,a,-b-c'),       &
 sg_info_type(  5,'a3   ','I2:a3 = I211        ','I 2 1 1    ',' I 2x            ','-b-c,a,c'),       &
 sg_info_type(  6,'b    ','Pm:b = P1m1         ','P m        ',' P -2y           ','a,b,c   '),       &
 sg_info_type(  6,'c    ','Pm:c = P11m         ','P 1 1 m    ',' P -2            ','b,c,a   '),       &
 sg_info_type(  6,'a    ','Pm:a = Pm11         ','P m 1 1    ',' P -2x           ','c,a,b   '),       &
 sg_info_type(  7,'b1   ','Pc:b1 = P1c1        ','P c        ',' P -2yc          ','a,b,c   '),       &
 sg_info_type(  7,'b2   ','Pn:b2 = P1n1        ','P n        ',' P -2yac         ','c,b,-a-c'),       &
 sg_info_type(  7,'b3   ','Pa:b3 = P1a1        ','P a        ',' P -2ya          ','-a-c,b,a'),       &
 sg_info_type(  7,'c1   ','Pa:c1 = P11a        ','P 1 1 a    ',' P -2a           ','b,c,a   '),       &
 sg_info_type(  7,'c2   ','Pn:c2 = P11n        ','P 1 1 n    ',' P -2ab          ','a,c,-a-b'),       &
 sg_info_type(  7,'c3   ','Pb:c3 = P11b        ','P 1 1 b    ',' P -2b           ','-a-b,c,b'),       &
 sg_info_type(  7,'a1   ','Pb:a1 = Pb11        ','P b 1 1    ',' P -2xb          ','c,a,b   '),       &
 sg_info_type(  7,'a2   ','Pn:a2 = Pn11        ','P n 1 1    ',' P -2xbc         ','b,a,-b-c'),       &
 sg_info_type(  7,'a3   ','Pc:a3 = Pc11        ','P c 1 1    ',' P -2xc          ','-b-c,a,c'),       &
 sg_info_type(  8,'b1   ','Cm:b1 = C1m1        ','C m        ',' C -2y           ','        '),       &
 sg_info_type(  8,'b2   ','Am:b2 = A1m1        ','A m        ',' A -2y           ','        '),       &
 sg_info_type(  8,'b3   ','Im:b3 = I1m1        ','I m        ',' I -2y           ','        '),       &
 sg_info_type(  8,'c1   ','Am:c1 = A11m        ','A 1 1 m    ',' A -2            ','        '),       &
 sg_info_type(  8,'c2   ','Bm:c2 = B11m        ','B 1 1 m    ',' B -2            ','        '),       &
 sg_info_type(  8,'c3   ','Im:c3 = I11m        ','I 1 1 m    ',' I -2            ','        '),       &
 sg_info_type(  8,'a1   ','Bm:a1 = Bm11        ','B m 1 1    ',' B -2x           ','        '),       &
 sg_info_type(  8,'a2   ','Cm:a2 = Cm11        ','C m 1 1    ',' C -2x           ','        '),       &
 sg_info_type(  8,'a3   ','Im:a3 = Im11        ','I m 1 1    ',' I -2x           ','        '),       &
 sg_info_type(  9,'b1   ','Cc:b1 = C1c1        ','C c        ',' C -2yc          ','        '),       &
 sg_info_type(  9,'b2   ','An:b2 = A1n1        ','A n        ',' A -2yac         ','        '),       &
 sg_info_type(  9,'b3   ','Ia:b3 = I1a1        ','I a        ',' I -2ya          ','        '),       &
 sg_info_type(  9,'-b1  ','Aa:-b1 = A1a1       ','A a        ',' A -2ya          ','        '),       &
 sg_info_type(  9,'-b2  ','Cn:-b2 = C1n1       ','C n        ',' C -2ybc         ','        '),       &
 sg_info_type(  9,'-b3  ','Ic:-b3 = I1c1       ','I c        ',' I -2yc          ','        '),       &
 sg_info_type(  9,'c1   ','Aa:c1 = A11a        ','A 1 1 a    ',' A -2a           ','        '),       &
 sg_info_type(  9,'c2   ','Bn:c2 = B11n        ','B 1 1 n    ',' B -2bc          ','        '),       &
 sg_info_type(  9,'c3   ','Ib:c3 = I11b        ','I 1 1 b    ',' I -2b           ','        '),       &
 sg_info_type(  9,'-c1  ','Bb:-c1 = B11b       ','B 1 1 b    ',' B -2b           ','        '),       &
 sg_info_type(  9,'-c2  ','An:-c2 = A11n       ','A 1 1 n    ',' A -2ac          ','        '),       &
 sg_info_type(  9,'-c3  ','Ia:-c3 = I11a       ','I 1 1 a    ',' I -2a           ','        '),       &
 sg_info_type(  9,'a1   ','Bb:a1 = Bb11        ','B b 1 1    ',' B -2xb          ','        '),       &
 sg_info_type(  9,'a2   ','Cn:a2 = Cn11        ','C n 1 1    ',' C -2xbc         ','        '),       &
 sg_info_type(  9,'a3   ','Ic:a3 = Ic11        ','I c 1 1    ',' I -2xc          ','        '),       &
 sg_info_type(  9,'-a1  ','Cc:-a1 = Cc11       ','C c 1 1    ',' C -2xc          ','        '),       &
 sg_info_type(  9,'-a2  ','Bn:-a2 = Bn11       ','B n 1 1    ',' B -2xbc         ','        '),       &
 sg_info_type(  9,'-a3  ','Ib:-a3 = Ib11       ','I b 1 1    ',' I -2xb          ','        '),       &
 sg_info_type( 10,'b    ','P2/m:b = P12/m1     ','P 2/m      ','-P 2y            ','        '),       &
 sg_info_type( 10,'c    ','P2/m:c = P112/m     ','P 1 1 2/m  ','-P 2             ','        '),       &
 sg_info_type( 10,'a    ','P2/m:a = P2/m11     ','P 2/m 1 1  ','-P 2x            ','        '),       &
 sg_info_type( 11,'b    ','P21/m:b = P121/m1   ','P 21/m     ','-P 2yb           ','        '),       &
 sg_info_type( 11,'c    ','P21/m:c = P1121/m   ','P 1 1 21/m ','-P 2c            ','        '),       &
 sg_info_type( 11,'a    ','P21/m:a = P21/m11   ','P 21/m 1 1 ','-P 2xa           ','        '),       &
 sg_info_type( 12,'b1   ','C2/m:b1 = C12/m1    ','C 2/m      ','-C 2y            ','        '),       &
 sg_info_type( 12,'b2   ','A2/m:b2 = A12/m1    ','A 2/m      ','-A 2y            ','        '),       &
 sg_info_type( 12,'b3   ','I2/m:b3 = I12/m1    ','I 2/m      ','-I 2y            ','        '),       &
 sg_info_type( 12,'c1   ','A2/m:c1 = A112/m    ','A 1 1 2/m  ','-A 2             ','        '),       &
 sg_info_type( 12,'c2   ','B2/m:c2 = B112/m    ','B 1 1 2/m  ','-B 2             ','        '),       &
 sg_info_type( 12,'c3   ','I2/m:c3 = I112/m    ','I 1 1 2/m  ','-I 2             ','        '),       &
 sg_info_type( 12,'a1   ','B2/m:a1 = B2/m11    ','B 2/m 1 1  ','-B 2x            ','        '),       &
 sg_info_type( 12,'a2   ','C2/m:a2 = C2/m11    ','C 2/m 1 1  ','-C 2x            ','        '),       &
 sg_info_type( 12,'a3   ','I2/m:a3 = I2/m11    ','I 2/m 1 1  ','-I 2x            ','        '),       &
 sg_info_type( 13,'b1   ','P2/c:b1 = P12/c1    ','P 2/c      ','-P 2yc           ','        '),       &
 sg_info_type( 13,'b2   ','P2/n:b2 = P12/n1    ','P 2/n      ','-P 2yac          ','        '),       &
 sg_info_type( 13,'b3   ','P2/a:b3 = P12/a1    ','P 2/a      ','-P 2ya           ','        '),       &
 sg_info_type( 13,'c1   ','P2/a:c1 = P112/a    ','P 1 1 2/a  ','-P 2a            ','        '),       &
 sg_info_type( 13,'c2   ','P2/n:c2 = P112/n    ','P 1 1 2/n  ','-P 2ab           ','        '),       &
 sg_info_type( 13,'c3   ','P2/b:c3 = P112/b    ','P 1 1 2/b  ','-P 2b            ','        '),       &
 sg_info_type( 13,'a1   ','P2/b:a1 = P2/b11    ','P 2/b 1 1  ','-P 2xb           ','        '),       &
 sg_info_type( 13,'a2   ','P2/n:a2 = P2/n11    ','P 2/n 1 1  ','-P 2xbc          ','        '),       &
 sg_info_type( 13,'a3   ','P2/c:a3 = P2/c11    ','P 2/c 1 1  ','-P 2xc           ','        '),       &
 sg_info_type( 14,'b1   ','P21/c:b1 = P121/c1  ','P 21/c     ','-P 2ybc          ','a,b,c   '),       &
 sg_info_type( 14,'b2   ','P21/n:b2 = P121/n1  ','P 21/n     ','-P 2yn           ','c,b,-a-c'),       &
 sg_info_type( 14,'b3   ','P21/a:b3 = P121/a1  ','P 21/a     ','-P 2yab          ','-a-c,b,a'),       &
 sg_info_type( 14,'c1   ','P21/a:c1 = P1121/a  ','P 1 1 21/a ','-P 2ac           ','b,c,a   '),       &
 sg_info_type( 14,'c2   ','P21/n:c2 = P1121/n  ','P 1 1 21/n ','-P 2n            ','a,c,-a-b'),       &
 sg_info_type( 14,'c3   ','P21/b:c3 = P1121/b  ','P 1 1 21/b ','-P 2bc           ','-a-b,c,b'),       &
 sg_info_type( 14,'a1   ','P21/b:a1 = P21/b11  ','P 21/b 1 1 ','-P 2xab          ','c,a,b   '),       &
 sg_info_type( 14,'a2   ','P21/n:a2 = P21/n11  ','P 21/n 1 1 ','-P 2xn           ','b,a,-b-c'),       &
 sg_info_type( 14,'a3   ','P21/c:a3 = P21/c11  ','P 21/c 1 1 ','-P 2xac          ','-b-c,a,c'),       &
 sg_info_type( 15,'b1   ','C2/c:b1 = C12/c1    ','C 2/c      ','-C 2yc           ','        '),       &
 sg_info_type( 15,'b2   ','A2/n:b2 = A12/n1    ','A 2/n      ','-A 2yac          ','        '),       &
 sg_info_type( 15,'b3   ','I2/a:b3 = I12/a1    ','I 2/a      ','-I 2ya           ','        '),       &
 sg_info_type( 15,'-b1  ','A2/a:-b1 = A12/a1   ','A 2/a      ','-A 2ya           ','        '),       &
 sg_info_type( 15,'-b2  ','C2/n:-b2 = C12/n1   ','C 2/n      ','-C 2ybc          ','        '),       &
 sg_info_type( 15,'-b3  ','I2/c:-b3 = I12/c1   ','I 2/c      ','-I 2yc           ','        '),       &
 sg_info_type( 15,'c1   ','A2/a:c1 = A112/a    ','A 1 1 2/a  ','-A 2a            ','        '),       &
 sg_info_type( 15,'c2   ','B2/n:c2 = B112/n    ','B 1 1 2/n  ','-B 2bc           ','        '),       &
 sg_info_type( 15,'c3   ','I2/b:c3 = I112/b    ','I 1 1 2/b  ','-I 2b            ','        '),       &
 sg_info_type( 15,'-c1  ','B2/b:-c1 = B112/b   ','B 1 1 2/b  ','-B 2b            ','        '),       &
 sg_info_type( 15,'-c2  ','A2/n:-c2 = A112/n   ','A 1 1 2/n  ','-A 2ac           ','        '),       &
 sg_info_type( 15,'-c3  ','I2/a:-c3 = I112/a   ','I 1 1 2/a  ','-I 2a            ','        '),       &
 sg_info_type( 15,'a1   ','B2/b:a1 = B2/b11    ','B 2/b 1 1  ','-B 2xb           ','        '),       &
 sg_info_type( 15,'a2   ','C2/n:a2 = C2/n11    ','C 2/n 1 1  ','-C 2xbc          ','        '),       &
 sg_info_type( 15,'a3   ','I2/c:a3 = I2/c11    ','I 2/c 1 1  ','-I 2xc           ','        '),       &
 sg_info_type( 15,'-a1  ','C2/c:-a1 = C2/c11   ','C 2/c 1 1  ','-C 2xc           ','        '),       &
 sg_info_type( 15,'-a2  ','B2/n:-a2 = B2/n11   ','B 2/n 1 1  ','-B 2xbc          ','        '),       &
 sg_info_type( 15,'-a3  ','I2/b:-a3 = I2/b11   ','I 2/b 1 1  ','-I 2xb           ','        '),       &
 sg_info_type( 16,'     ','P222                ','P 2 2 2    ',' P 2 2           ','        '),       &
 sg_info_type( 17,'     ','P2221               ','P 2 2 21   ',' P 2c 2          ','        '),       &
 sg_info_type( 17,'cab  ','P2122               ','P 21 2 2   ',' P 2a 2a         ','        '),       &
 sg_info_type( 17,'bca  ','P2212               ','P 2 21 2   ',' P 2 2b          ','        '),       &
 sg_info_type( 18,'     ','P21212              ','P 21 21 2  ',' P 2 2ab         ','        '),       &
 sg_info_type( 18,'cab  ','P22121              ','P 2 21 21  ',' P 2bc 2         ','        '),       &
 sg_info_type( 18,'bca  ','P21221              ','P 21 2 21  ',' P 2ac 2ac       ','        '),       &
 sg_info_type( 19,'     ','P212121             ','P 21 21 21 ',' P 2ac 2ab       ','        '),       &
 sg_info_type( 20,'     ','C2221               ','C 2 2 21   ',' C 2c 2          ','        '),       &
 sg_info_type( 20,'cab  ','A2122               ','A 21 2 2   ',' A 2a 2a         ','        '),       &
 sg_info_type( 20,'bca  ','B2212               ','B 2 21 2   ',' B 2 2b          ','        '),       &
 sg_info_type( 21,'     ','C222                ','C 2 2 2    ',' C 2 2           ','        '),       &
 sg_info_type( 21,'cab  ','A222                ','A 2 2 2    ',' A 2 2           ','        '),       &
 sg_info_type( 21,'bca  ','B222                ','B 2 2 2    ',' B 2 2           ','        '),       &
 sg_info_type( 22,'     ','F222                ','F 2 2 2    ',' F 2 2           ','        '),       &
 sg_info_type( 23,'     ','I222                ','I 2 2 2    ',' I 2 2           ','        '),       &
 sg_info_type( 24,'     ','I212121             ','I 21 21 21 ',' I 2b 2c         ','        '),       &
 sg_info_type( 25,'     ','Pmm2                ','P m m 2    ',' P 2 -2          ','        '),       &
 sg_info_type( 25,'cab  ','P2mm                ','P 2 m m    ',' P -2 2          ','        '),       &
 sg_info_type( 25,'bca  ','Pm2m                ','P m 2 m    ',' P -2 -2         ','        '),       &
 sg_info_type( 26,'     ','Pmc21               ','P m c 21   ',' P 2c -2         ','        '),       &
 sg_info_type( 26,'ba-c ','Pcm21               ','P c m 21   ',' P 2c -2c        ','        '),       &
 sg_info_type( 26,'cab  ','P21ma               ','P 21 m a   ',' P -2a 2a        ','        '),       &
 sg_info_type( 26,'-cba ','P21am               ','P 21 a m   ',' P -2 2a         ','        '),       &
 sg_info_type( 26,'bca  ','Pb21m               ','P b 21 m   ',' P -2 -2b        ','        '),       &
 sg_info_type( 26,'a-cb ','Pm21b               ','P m 21 b   ',' P -2b -2        ','        '),       &
 sg_info_type( 27,'     ','Pcc2                ','P c c 2    ',' P 2 -2c         ','        '),       &
 sg_info_type( 27,'cab  ','P2aa                ','P 2 a a    ',' P -2a 2         ','        '),       &
 sg_info_type( 27,'bca  ','Pb2b                ','P b 2 b    ',' P -2b -2b       ','        '),       &
 sg_info_type( 28,'     ','Pma2                ','P m a 2    ',' P 2 -2a         ','        '),       &
 sg_info_type( 28,'ba-c ','Pbm2                ','P b m 2    ',' P 2 -2b         ','        '),       &
 sg_info_type( 28,'cab  ','P2mb                ','P 2 m b    ',' P -2b 2         ','        '),       &
 sg_info_type( 28,'-cba ','P2cm                ','P 2 c m    ',' P -2c 2         ','        '),       &
 sg_info_type( 28,'bca  ','Pc2m                ','P c 2 m    ',' P -2c -2c       ','        '),       &
 sg_info_type( 28,'a-cb ','Pm2a                ','P m 2 a    ',' P -2a -2a       ','        '),       &
 sg_info_type( 29,'     ','Pca21               ','P c a 21   ',' P 2c -2ac       ','        '),       &
 sg_info_type( 29,'ba-c ','Pbc21               ','P b c 21   ',' P 2c -2b        ','        '),       &
 sg_info_type( 29,'cab  ','P21ab               ','P 21 a b   ',' P -2b 2a        ','        '),       &
 sg_info_type( 29,'-cba ','P21ca               ','P 21 c a   ',' P -2ac 2a       ','        '),       &
 sg_info_type( 29,'bca  ','Pc21b               ','P c 21 b   ',' P -2bc -2c      ','        '),       &
 sg_info_type( 29,'a-cb ','Pb21a               ','P b 21 a   ',' P -2a -2ab      ','        '),       &
 sg_info_type( 30,'     ','Pnc2                ','P n c 2    ',' P 2 -2bc        ','        '),       &
 sg_info_type( 30,'ba-c ','Pcn2                ','P c n 2    ',' P 2 -2ac        ','        '),       &
 sg_info_type( 30,'cab  ','P2na                ','P 2 n a    ',' P -2ac 2        ','        '),       &
 sg_info_type( 30,'-cba ','P2an                ','P 2 a n    ',' P -2ab 2        ','        '),       &
 sg_info_type( 30,'bca  ','Pb2n                ','P b 2 n    ',' P -2ab -2ab     ','        '),       &
 sg_info_type( 30,'a-cb ','Pn2b                ','P n 2 b    ',' P -2bc -2bc     ','        '),       &
 sg_info_type( 31,'     ','Pmn21               ','P m n 21   ',' P 2ac -2        ','        '),       &
 sg_info_type( 31,'ba-c ','Pnm21               ','P n m 21   ',' P 2bc -2bc      ','        '),       &
 sg_info_type( 31,'cab  ','P21mn               ','P 21 m n   ',' P -2ab 2ab      ','        '),       &
 sg_info_type( 31,'-cba ','P21nm               ','P 21 n m   ',' P -2 2ac        ','        '),       &
 sg_info_type( 31,'bca  ','Pn21m               ','P n 21 m   ',' P -2 -2bc       ','        '),       &
 sg_info_type( 31,'a-cb ','Pm21n               ','P m 21 n   ',' P -2ab -2       ','        '),       &
 sg_info_type( 32,'     ','Pba2                ','P b a 2    ',' P 2 -2ab        ','        '),       &
 sg_info_type( 32,'cab  ','P2cb                ','P 2 c b    ',' P -2bc 2        ','        '),       &
 sg_info_type( 32,'bca  ','Pc2a                ','P c 2 a    ',' P -2ac -2ac     ','        '),       &
 sg_info_type( 33,'     ','Pna21               ','P n a 21   ',' P 2c -2n        ','        '),       &
 sg_info_type( 33,'ba-c ','Pbn21               ','P b n 21   ',' P 2c -2ab       ','        '),       &
 sg_info_type( 33,'cab  ','P21nb               ','P 21 n b   ',' P -2bc 2a       ','        '),       &
 sg_info_type( 33,'-cba ','P21cn               ','P 21 c n   ',' P -2n 2a        ','        '),       &
 sg_info_type( 33,'bca  ','Pc21n               ','P c 21 n   ',' P -2n -2ac      ','        '),       &
 sg_info_type( 33,'a-cb ','Pn21a               ','P n 21 a   ',' P -2ac -2n      ','        '),       &
 sg_info_type( 34,'     ','Pnn2                ','P n n 2    ',' P 2 -2n         ','        '),       &
 sg_info_type( 34,'cab  ','P2nn                ','P 2 n n    ',' P -2n 2         ','        '),       &
 sg_info_type( 34,'bca  ','Pn2n                ','P n 2 n    ',' P -2n -2n       ','        '),       &
 sg_info_type( 35,'     ','Cmm2                ','C m m 2    ',' C 2 -2          ','        '),       &
 sg_info_type( 35,'cab  ','A2mm                ','A 2 m m    ',' A -2 2          ','        '),       &
 sg_info_type( 35,'bca  ','Bm2m                ','B m 2 m    ',' B -2 -2         ','        '),       &
 sg_info_type( 36,'     ','Cmc21               ','C m c 21   ',' C 2c -2         ','        '),       &
 sg_info_type( 36,'ba-c ','Ccm21               ','C c m 21   ',' C 2c -2c        ','        '),       &
 sg_info_type( 36,'cab  ','A21ma               ','A 21 m a   ',' A -2a 2a        ','        '),       &
 sg_info_type( 36,'-cba ','A21am               ','A 21 a m   ',' A -2 2a         ','        '),       &
 sg_info_type( 36,'bca  ','Bb21m               ','B b 21 m   ',' B -2 -2b        ','        '),       &
 sg_info_type( 36,'a-cb ','Bm21b               ','B m 21 b   ',' B -2b -2        ','        '),       &
 sg_info_type( 37,'     ','Ccc2                ','C c c 2    ',' C 2 -2c         ','        '),       &
 sg_info_type( 37,'cab  ','A2aa                ','A 2 a a    ',' A -2a 2         ','        '),       &
 sg_info_type( 37,'bca  ','Bb2b                ','B b 2 b    ',' B -2b -2b       ','        '),       &
 sg_info_type( 38,'     ','Amm2                ','A m m 2    ',' A 2 -2          ','        '),       &
 sg_info_type( 38,'ba-c ','Bmm2                ','B m m 2    ',' B 2 -2          ','        '),       &
 sg_info_type( 38,'cab  ','B2mm                ','B 2 m m    ',' B -2 2          ','        '),       &
 sg_info_type( 38,'-cba ','C2mm                ','C 2 m m    ',' C -2 2          ','        '),       &
 sg_info_type( 38,'bca  ','Cm2m                ','C m 2 m    ',' C -2 -2         ','        '),       &
 sg_info_type( 38,'a-cb ','Am2m                ','A m 2 m    ',' A -2 -2         ','        '),       &
 sg_info_type( 39,'     ','Abm2                ','A b m 2    ',' A 2 -2c         ','        '),       &
 sg_info_type( 39,'ba-c ','Bma2                ','B m a 2    ',' B 2 -2c         ','        '),       &
 sg_info_type( 39,'cab  ','B2cm                ','B 2 c m    ',' B -2c 2         ','        '),       &
 sg_info_type( 39,'-cba ','C2mb                ','C 2 m b    ',' C -2b 2         ','        '),       &
 sg_info_type( 39,'bca  ','Cm2a                ','C m 2 a    ',' C -2b -2b       ','        '),       &
 sg_info_type( 39,'a-cb ','Ac2m                ','A c 2 m    ',' A -2c -2c       ','        '),       &
 sg_info_type( 40,'     ','Ama2                ','A m a 2    ',' A 2 -2a         ','        '),       &
 sg_info_type( 40,'ba-c ','Bbm2                ','B b m 2    ',' B 2 -2b         ','        '),       &
 sg_info_type( 40,'cab  ','B2mb                ','B 2 m b    ',' B -2b 2         ','        '),       &
 sg_info_type( 40,'-cba ','C2cm                ','C 2 c m    ',' C -2c 2         ','        '),       &
 sg_info_type( 40,'bca  ','Cc2m                ','C c 2 m    ',' C -2c -2c       ','        '),       &
 sg_info_type( 40,'a-cb ','Am2a                ','A m 2 a    ',' A -2a -2a       ','        '),       &
 sg_info_type( 41,'     ','Aba2                ','A b a 2    ',' A 2 -2ac        ','        '),       &
 sg_info_type( 41,'ba-c ','Bba2                ','B b a 2    ',' B 2 -2bc        ','        '),       &
 sg_info_type( 41,'cab  ','B2cb                ','B 2 c b    ',' B -2bc 2        ','        '),       &
 sg_info_type( 41,'-cba ','C2cb                ','C 2 c b    ',' C -2bc 2        ','        '),       &
 sg_info_type( 41,'bca  ','Cc2a                ','C c 2 a    ',' C -2bc -2bc     ','        '),       &
 sg_info_type( 41,'a-cb ','Ac2a                ','A c 2 a    ',' A -2ac -2ac     ','        '),       &
 sg_info_type( 42,'     ','Fmm2                ','F m m 2    ',' F 2 -2          ','        '),       &
 sg_info_type( 42,'cab  ','F2mm                ','F 2 m m    ',' F -2 2          ','        '),       &
 sg_info_type( 42,'bca  ','Fm2m                ','F m 2 m    ',' F -2 -2         ','        '),       &
 sg_info_type( 43,'     ','Fdd2                ','F d d 2    ',' F 2 -2d         ','        '),       &
 sg_info_type( 43,'cab  ','F2dd                ','F 2 d d    ',' F -2d 2         ','        '),       &
 sg_info_type( 43,'bca  ','Fd2d                ','F d 2 d    ',' F -2d -2d       ','        '),       &
 sg_info_type( 44,'     ','Imm2                ','I m m 2    ',' I 2 -2          ','        '),       &
 sg_info_type( 44,'cab  ','I2mm                ','I 2 m m    ',' I -2 2          ','        '),       &
 sg_info_type( 44,'bca  ','Im2m                ','I m 2 m    ',' I -2 -2         ','        '),       &
 sg_info_type( 45,'     ','Iba2                ','I b a 2    ',' I 2 -2c         ','        '),       &
 sg_info_type( 45,'cab  ','I2cb                ','I 2 c b    ',' I -2a 2         ','        '),       &
 sg_info_type( 45,'bca  ','Ic2a                ','I c 2 a    ',' I -2b -2b       ','        '),       &
 sg_info_type( 46,'     ','Ima2                ','I m a 2    ',' I 2 -2a         ','        '),       &
 sg_info_type( 46,'ba-c ','Ibm2                ','I b m 2    ',' I 2 -2b         ','        '),       &
 sg_info_type( 46,'cab  ','I2mb                ','I 2 m b    ',' I -2b 2         ','        '),       &
 sg_info_type( 46,'-cba ','I2cm                ','I 2 c m    ',' I -2c 2         ','        '),       &
 sg_info_type( 46,'bca  ','Ic2m                ','I c 2 m    ',' I -2c -2c       ','        '),       &
 sg_info_type( 46,'a-cb ','Im2a                ','I m 2 a    ',' I -2a -2a       ','        '),       &
 sg_info_type( 47,'     ','Pmmm                ','P m m m    ','-P 2 2           ','        '),       &
 sg_info_type( 48,'1    ','Pnnn:1              ','P n n n    ',' P 2 2 -1n       ','        '),       &
 sg_info_type( 48,'2    ','Pnnn:2              ','P n n n    ','-P 2ab 2bc       ','        '),       &
 sg_info_type( 49,'     ','Pccm                ','P c c m    ','-P 2 2c          ','        '),       &
 sg_info_type( 49,'cab  ','Pmaa                ','P m a a    ','-P 2a 2          ','        '),       &
 sg_info_type( 49,'bca  ','Pbmb                ','P b m b    ','-P 2b 2b         ','        '),       &
 sg_info_type( 50,'1    ','Pban:1              ','P b a n    ',' P 2 2 -1ab      ','        '),       &
 sg_info_type( 50,'2    ','Pban:2              ','P b a n    ','-P 2ab 2b        ','        '),       &
 sg_info_type( 50,'1cab ','Pncb:1              ','P n c b    ',' P 2 2 -1bc      ','        '),       &
 sg_info_type( 50,'2cab ','Pncb:2              ','P n c b    ','-P 2b 2bc        ','        '),       &
 sg_info_type( 50,'1bca ','Pcna:1              ','P c n a    ',' P 2 2 -1ac      ','        '),       &
 sg_info_type( 50,'2bca ','Pcna:2              ','P c n a    ','-P 2a 2c         ','        '),       &
 sg_info_type( 51,'     ','Pmma                ','P m m a    ','-P 2a 2a         ','        '),       &
 sg_info_type( 51,'ba-c ','Pmmb                ','P m m b    ','-P 2b 2          ','        '),       &
 sg_info_type( 51,'cab  ','Pbmm                ','P b m m    ','-P 2 2b          ','        '),       &
 sg_info_type( 51,'-cba ','Pcmm                ','P c m m    ','-P 2c 2c         ','        '),       &
 sg_info_type( 51,'bca  ','Pmcm                ','P m c m    ','-P 2c 2          ','        '),       &
 sg_info_type( 51,'a-cb ','Pmam                ','P m a m    ','-P 2 2a          ','        '),       &
 sg_info_type( 52,'     ','Pnna                ','P n n a    ','-P 2a 2bc        ','        '),       &
 sg_info_type( 52,'ba-c ','Pnnb                ','P n n b    ','-P 2b 2n         ','        '),       &
 sg_info_type( 52,'cab  ','Pbnn                ','P b n n    ','-P 2n 2b         ','        '),       &
 sg_info_type( 52,'-cba ','Pcnn                ','P c n n    ','-P 2ab 2c        ','        '),       &
 sg_info_type( 52,'bca  ','Pncn                ','P n c n    ','-P 2ab 2n        ','        '),       &
 sg_info_type( 52,'a-cb ','Pnan                ','P n a n    ','-P 2n 2bc        ','        '),       &
 sg_info_type( 53,'     ','Pmna                ','P m n a    ','-P 2ac 2         ','        '),       &
 sg_info_type( 53,'ba-c ','Pnmb                ','P n m b    ','-P 2bc 2bc       ','        '),       &
 sg_info_type( 53,'cab  ','Pbmn                ','P b m n    ','-P 2ab 2ab       ','        '),       &
 sg_info_type( 53,'-cba ','Pcnm                ','P c n m    ','-P 2 2ac         ','        '),       &
 sg_info_type( 53,'bca  ','Pncm                ','P n c m    ','-P 2 2bc         ','        '),       &
 sg_info_type( 53,'a-cb ','Pman                ','P m a n    ','-P 2ab 2         ','        '),       &
 sg_info_type( 54,'     ','Pcca                ','P c c a    ','-P 2a 2ac        ','        '),       &
 sg_info_type( 54,'ba-c ','Pccb                ','P c c b    ','-P 2b 2c         ','        '),       &
 sg_info_type( 54,'cab  ','Pbaa                ','P b a a    ','-P 2a 2b         ','        '),       &
 sg_info_type( 54,'-cba ','Pcaa                ','P c a a    ','-P 2ac 2c        ','        '),       &
 sg_info_type( 54,'bca  ','Pbcb                ','P b c b    ','-P 2bc 2b        ','        '),       &
 sg_info_type( 54,'a-cb ','Pbab                ','P b a b    ','-P 2b 2ab        ','        '),       &
 sg_info_type( 55,'     ','Pbam                ','P b a m    ','-P 2 2ab         ','        '),       &
 sg_info_type( 55,'cab  ','Pmcb                ','P m c b    ','-P 2bc 2         ','        '),       &
 sg_info_type( 55,'bca  ','Pcma                ','P c m a    ','-P 2ac 2ac       ','        '),       &
 sg_info_type( 56,'     ','Pccn                ','P c c n    ','-P 2ab 2ac       ','        '),       &
 sg_info_type( 56,'cab  ','Pnaa                ','P n a a    ','-P 2ac 2bc       ','        '),       &
 sg_info_type( 56,'bca  ','Pbnb                ','P b n b    ','-P 2bc 2ab       ','        '),       &
 sg_info_type( 57,'     ','Pbcm                ','P b c m    ','-P 2c 2b         ','        '),       &
 sg_info_type( 57,'ba-c ','Pcam                ','P c a m    ','-P 2c 2ac        ','        '),       &
 sg_info_type( 57,'cab  ','Pmca                ','P m c a    ','-P 2ac 2a        ','        '),       &
 sg_info_type( 57,'-cba ','Pmab                ','P m a b    ','-P 2b 2a         ','        '),       &
 sg_info_type( 57,'bca  ','Pbma                ','P b m a    ','-P 2a 2ab        ','        '),       &
 sg_info_type( 57,'a-cb ','Pcmb                ','P c m b    ','-P 2bc 2c        ','        '),       &
 sg_info_type( 58,'     ','Pnnm                ','P n n m    ','-P 2 2n          ','        '),       &
 sg_info_type( 58,'cab  ','Pmnn                ','P m n n    ','-P 2n 2          ','        '),       &
 sg_info_type( 58,'bca  ','Pnmn                ','P n m n    ','-P 2n 2n         ','        '),       &
 sg_info_type( 59,'1    ','Pmmn:1              ','P m m n    ',' P 2 2ab -1ab    ','        '),       &
 sg_info_type( 59,'2    ','Pmmn:2              ','P m m n    ','-P 2ab 2a        ','        '),       &
 sg_info_type( 59,'1cab ','Pnmm:1              ','P n m m    ',' P 2bc 2 -1bc    ','        '),       &
 sg_info_type( 59,'2cab ','Pnmm:2              ','P n m m    ','-P 2c 2bc        ','        '),       &
 sg_info_type( 59,'1bca ','Pmnm:1              ','P m n m    ',' P 2ac 2ac -1ac  ','        '),       &
 sg_info_type( 59,'2bca ','Pmnm:2              ','P m n m    ','-P 2c 2a         ','        '),       &
 sg_info_type( 60,'     ','Pbcn                ','P b c n    ','-P 2n 2ab        ','        '),       &
 sg_info_type( 60,'ba-c ','Pcan                ','P c a n    ','-P 2n 2c         ','        '),       &
 sg_info_type( 60,'cab  ','Pnca                ','P n c a    ','-P 2a 2n         ','        '),       &
 sg_info_type( 60,'-cba ','Pnab                ','P n a b    ','-P 2bc 2n        ','        '),       &
 sg_info_type( 60,'bca  ','Pbna                ','P b n a    ','-P 2ac 2b        ','        '),       &
 sg_info_type( 60,'a-cb ','Pcnb                ','P c n b    ','-P 2b 2ac        ','        '),       &
 sg_info_type( 61,'     ','Pbca                ','P b c a    ','-P 2ac 2ab       ','        '),       &
 sg_info_type( 61,'ba-c ','Pcab                ','P c a b    ','-P 2bc 2ac       ','        '),       &
 sg_info_type( 62,'     ','Pnma                ','P n m a    ','-P 2ac 2n        ','        '),       &
 sg_info_type( 62,'ba-c ','Pmnb                ','P m n b    ','-P 2bc 2a        ','        '),       &
 sg_info_type( 62,'cab  ','Pbnm                ','P b n m    ','-P 2c 2ab        ','        '),       &
 sg_info_type( 62,'-cba ','Pcmn                ','P c m n    ','-P 2n 2ac        ','        '),       &
 sg_info_type( 62,'bca  ','Pmcn                ','P m c n    ','-P 2n 2a         ','        '),       &
 sg_info_type( 62,'a-cb ','Pnam                ','P n a m    ','-P 2c 2n         ','        '),       &
 sg_info_type( 63,'     ','Cmcm                ','C m c m    ','-C 2c 2          ','        '),       &
 sg_info_type( 63,'ba-c ','Ccmm                ','C c m m    ','-C 2c 2c         ','        '),       &
 sg_info_type( 63,'cab  ','Amma                ','A m m a    ','-A 2a 2a         ','        '),       &
 sg_info_type( 63,'-cba ','Amam                ','A m a m    ','-A 2 2a          ','        '),       &
 sg_info_type( 63,'bca  ','Bbmm                ','B b m m    ','-B 2 2b          ','        '),       &
 sg_info_type( 63,'a-cb ','Bmmb                ','B m m b    ','-B 2b 2          ','        '),       &
 sg_info_type( 64,'     ','Cmca                ','C m c a    ','-C 2bc 2         ','        '),       &
 sg_info_type( 64,'ba-c ','Ccmb                ','C c m b    ','-C 2bc 2bc       ','        '),       &
 sg_info_type( 64,'cab  ','Abma                ','A b m a    ','-A 2ac 2ac       ','        '),       &
 sg_info_type( 64,'-cba ','Acam                ','A c a m    ','-A 2 2ac         ','        '),       &
 sg_info_type( 64,'bca  ','Bbcm                ','B b c m    ','-B 2 2bc         ','        '),       &
 sg_info_type( 64,'a-cb ','Bmab                ','B m a b    ','-B 2bc 2         ','        '),       &
 sg_info_type( 65,'     ','Cmmm                ','C m m m    ','-C 2 2           ','        '),       &
 sg_info_type( 65,'cab  ','Ammm                ','A m m m    ','-A 2 2           ','        '),       &
 sg_info_type( 65,'bca  ','Bmmm                ','B m m m    ','-B 2 2           ','        '),       &
 sg_info_type( 66,'     ','Cccm                ','C c c m    ','-C 2 2c          ','        '),       &
 sg_info_type( 66,'cab  ','Amaa                ','A m a a    ','-A 2a 2          ','        '),       &
 sg_info_type( 66,'bca  ','Bbmb                ','B b m b    ','-B 2b 2b         ','        '),       &
 sg_info_type( 67,'     ','Cmma                ','C m m a    ','-C 2b 2          ','        '),       &
 sg_info_type( 67,'ba-c ','Cmmb                ','C m m b    ','-C 2b 2b         ','        '),       &
 sg_info_type( 67,'cab  ','Abmm                ','A b m m    ','-A 2c 2c         ','        '),       &
 sg_info_type( 67,'-cba ','Acmm                ','A c m m    ','-A 2 2c          ','        '),       &
 sg_info_type( 67,'bca  ','Bmcm                ','B m c m    ','-B 2 2c          ','        '),       &
 sg_info_type( 67,'a-cb ','Bmam                ','B m a m    ','-B 2c 2          ','        '),       &
 sg_info_type( 68,'1    ','Ccca:1              ','C c c a    ',' C 2 2 -1bc      ','        '),       &
 sg_info_type( 68,'2    ','Ccca:2              ','C c c a    ','-C 2b 2bc        ','        '),       &
 sg_info_type( 68,'1ba-c','Cccb:1              ','C c c b    ',' C 2 2 -1bc      ','        '),       &
 sg_info_type( 68,'2ba-c','Cccb:2              ','C c c b    ','-C 2b 2c         ','        '),       &
 sg_info_type( 68,'1cab ','Abaa:1              ','A b a a    ',' A 2 2 -1ac      ','        '),       &
 sg_info_type( 68,'2cab ','Abaa:2              ','A b a a    ','-A 2a 2c         ','        '),       &
 sg_info_type( 68,'1-cba','Acaa:1              ','A c a a    ',' A 2 2 -1ac      ','        '),       &
 sg_info_type( 68,'2-cba','Acaa:2              ','A c a a    ','-A 2ac 2c        ','        '),       &
 sg_info_type( 68,'1bca ','Bbcb:1              ','B b c b    ',' B 2 2 -1bc      ','        '),       &
 sg_info_type( 68,'2bca ','Bbcb:2              ','B b c b    ','-B 2bc 2b        ','        '),       &
 sg_info_type( 68,'1a-cb','Bbab:1              ','B b a b    ',' B 2 2 -1bc      ','        '),       &
 sg_info_type( 68,'2a-cb','Bbab:2              ','B b a b    ','-B 2b 2bc        ','        '),       &
 sg_info_type( 69,'     ','Fmmm                ','F m m m    ','-F 2 2           ','        '),       &
 sg_info_type( 70,'1    ','Fddd:1              ','F d d d    ',' F 2 2 -1d       ','        '),       &
 sg_info_type( 70,'2    ','Fddd:2              ','F d d d    ','-F 2uv 2vw       ','        '),       &
 sg_info_type( 71,'     ','Immm                ','I m m m    ','-I 2 2           ','        '),       &
 sg_info_type( 72,'     ','Ibam                ','I b a m    ','-I 2 2c          ','        '),       &
 sg_info_type( 72,'cab  ','Imcb                ','I m c b    ','-I 2a 2          ','        '),       &
 sg_info_type( 72,'bca  ','Icma                ','I c m a    ','-I 2b 2b         ','        '),       &
 sg_info_type( 73,'     ','Ibca                ','I b c a    ','-I 2b 2c         ','        '),       &
 sg_info_type( 73,'ba-c ','Icab                ','I c a b    ','-I 2a 2b         ','        '),       &
 sg_info_type( 74,'     ','Imma                ','I m m a    ','-I 2b 2          ','        '),       &
 sg_info_type( 74,'ba-c ','Immb                ','I m m b    ','-I 2a 2a         ','        '),       &
 sg_info_type( 74,'cab  ','Ibmm                ','I b m m    ','-I 2c 2c         ','        '),       &
 sg_info_type( 74,'-cba ','Icmm                ','I c m m    ','-I 2 2b          ','        '),       &
 sg_info_type( 74,'bca  ','Imcm                ','I m c m    ','-I 2 2a          ','        '),       &
 sg_info_type( 74,'a-cb ','Imam                ','I m a m    ','-I 2c 2          ','        '),       &
 sg_info_type( 75,'     ','P4                  ','P 4        ',' P 4             ','        '),       &
 sg_info_type( 76,'     ','P41                 ','P 41       ',' P 4w            ','        '),       &
 sg_info_type( 77,'     ','P42                 ','P 42       ',' P 4c            ','        '),       &
 sg_info_type( 78,'     ','P43                 ','P 43       ',' P 4cw           ','        '),       &
 sg_info_type( 79,'     ','I4                  ','I 4        ',' I 4             ','        '),       &
 sg_info_type( 80,'     ','I41                 ','I 41       ',' I 4bw           ','        '),       &
 sg_info_type( 81,'     ','P-4                 ','P -4       ',' P -4            ','        '),       &
 sg_info_type( 82,'     ','I-4                 ','I -4       ',' I -4            ','        '),       &
 sg_info_type( 83,'     ','P4/m                ','P 4/m      ','-P 4             ','        '),       &
 sg_info_type( 84,'     ','P42/m               ','P 42/m     ','-P 4c            ','        '),       &
 sg_info_type( 85,'1    ','P4/n:1              ','P 4/n      ',' P 4ab -1ab      ','        '),       &
 sg_info_type( 85,'2    ','P4/n:2              ','P 4/n      ','-P 4a            ','        '),       &
 sg_info_type( 86,'1    ','P42/n:1             ','P 42/n     ',' P 4n -1n        ','        '),       &
 sg_info_type( 86,'2    ','P42/n:2             ','P 42/n     ','-P 4bc           ','        '),       &
 sg_info_type( 87,'     ','I4/m                ','I 4/m      ','-I 4             ','        '),       &
 sg_info_type( 88,'1    ','I41/a:1             ','I 41/a     ',' I 4bw -1bw      ','        '),       &
 sg_info_type( 88,'2    ','I41/a:2             ','I 41/a     ','-I 4ad           ','        '),       &
 sg_info_type( 89,'     ','P422                ','P 4 2 2    ',' P 4 2           ','        '),       &
 sg_info_type( 90,'     ','P4212               ','P 4 21 2   ',' P 4ab 2ab       ','        '),       &
 sg_info_type( 91,'     ','P4122               ','P 41 2 2   ',' P 4w 2c         ','        '),       &
 sg_info_type( 92,'     ','P41212              ','P 41 21 2  ',' P 4abw 2nw      ','        '),       &
 sg_info_type( 93,'     ','P4222               ','P 42 2 2   ',' P 4c 2          ','        '),       &
 sg_info_type( 94,'     ','P42212              ','P 42 21 2  ',' P 4n 2n         ','        '),       &
 sg_info_type( 95,'     ','P4322               ','P 43 2 2   ',' P 4cw 2c        ','        '),       &
 sg_info_type( 96,'     ','P43212              ','P 43 21 2  ',' P 4nw 2abw      ','        '),       &
 sg_info_type( 97,'     ','I422                ','I 4 2 2    ',' I 4 2           ','        '),       &
 sg_info_type( 98,'     ','I4122               ','I 41 2 2   ',' I 4bw 2bw       ','        '),       &
 sg_info_type( 99,'     ','P4mm                ','P 4 m m    ',' P 4 -2          ','        '),       &
 sg_info_type(100,'     ','P4bm                ','P 4 b m    ',' P 4 -2ab        ','        '),       &
 sg_info_type(101,'     ','P42cm               ','P 42 c m   ',' P 4c -2c        ','        '),       &
 sg_info_type(102,'     ','P42nm               ','P 42 n m   ',' P 4n -2n        ','        '),       &
 sg_info_type(103,'     ','P4cc                ','P 4 c c    ',' P 4 -2c         ','        '),       &
 sg_info_type(104,'     ','P4nc                ','P 4 n c    ',' P 4 -2n         ','        '),       &
 sg_info_type(105,'     ','P42mc               ','P 42 m c   ',' P 4c -2         ','        '),       &
 sg_info_type(106,'     ','P42bc               ','P 42 b c   ',' P 4c -2ab       ','        '),       &
 sg_info_type(107,'     ','I4mm                ','I 4 m m    ',' I 4 -2          ','        '),       &
 sg_info_type(108,'     ','I4cm                ','I 4 c m    ',' I 4 -2c         ','        '),       &
 sg_info_type(109,'     ','I41md               ','I 41 m d   ',' I 4bw -2        ','        '),       &
 sg_info_type(110,'     ','I41cd               ','I 41 c d   ',' I 4bw -2c       ','        '),       &
 sg_info_type(111,'     ','P-42m               ','P -4 2 m   ',' P -4 2          ','        '),       &
 sg_info_type(112,'     ','P-42c               ','P -4 2 c   ',' P -4 2c         ','        '),       &
 sg_info_type(113,'     ','P-421m              ','P -4 21 m  ',' P -4 2ab        ','        '),       &
 sg_info_type(114,'     ','P-421c              ','P -4 21 c  ',' P -4 2n         ','        '),       &
 sg_info_type(115,'     ','P-4m2               ','P -4 m 2   ',' P -4 -2         ','        '),       &
 sg_info_type(116,'     ','P-4c2               ','P -4 c 2   ',' P -4 -2c        ','        '),       &
 sg_info_type(117,'     ','P-4b2               ','P -4 b 2   ',' P -4 -2ab       ','        '),       &
 sg_info_type(118,'     ','P-4n2               ','P -4 n 2   ',' P -4 -2n        ','        '),       &
 sg_info_type(119,'     ','I-4m2               ','I -4 m 2   ',' I -4 -2         ','        '),       &
 sg_info_type(120,'     ','I-4c2               ','I -4 c 2   ',' I -4 -2c        ','        '),       &
 sg_info_type(121,'     ','I-42m               ','I -4 2 m   ',' I -4 2          ','        '),       &
 sg_info_type(122,'     ','I-42d               ','I -4 2 d   ',' I -4 2bw        ','        '),       &
 sg_info_type(123,'     ','P4/mmm              ','P 4/m m m  ','-P 4 2           ','        '),       &
 sg_info_type(124,'     ','P4/mcc              ','P 4/m c c  ','-P 4 2c          ','        '),       &
 sg_info_type(125,'1    ','P4/nbm:1            ','P 4/n b m  ',' P 4 2 -1ab      ','        '),       &
 sg_info_type(125,'2    ','P4/nbm:2            ','P 4/n b m  ','-P 4a 2b         ','        '),       &
 sg_info_type(126,'1    ','P4/nnc:1            ','P 4/n n c  ',' P 4 2 -1n       ','        '),       &
 sg_info_type(126,'2    ','P4/nnc:2            ','P 4/n n c  ','-P 4a 2bc        ','        '),       &
 sg_info_type(127,'     ','P4/mbm              ','P 4/m b m  ','-P 4 2ab         ','        '),       &
 sg_info_type(128,'     ','P4/mnc              ','P 4/m n c  ','-P 4 2n          ','        '),       &
 sg_info_type(129,'1    ','P4/nmm:1            ','P 4/n m m  ',' P 4ab 2ab -1ab  ','        '),       &
 sg_info_type(129,'2    ','P4/nmm:2            ','P 4/n m m  ','-P 4a 2a         ','        '),       &
 sg_info_type(130,'1    ','P4/ncc:1            ','P 4/n c c  ',' P 4ab 2n -1ab   ','        '),       &
 sg_info_type(130,'2    ','P4/ncc:2            ','P 4/n c c  ','-P 4a 2ac        ','        '),       &
 sg_info_type(131,'     ','P42/mmc             ','P 42/m m c ','-P 4c 2          ','        '),       &
 sg_info_type(132,'     ','P42/mcm             ','P 42/m c m ','-P 4c 2c         ','        '),       &
 sg_info_type(133,'1    ','P42/nbc:1           ','P 42/n b c ',' P 4n 2c -1n     ','        '),       &
 sg_info_type(133,'2    ','P42/nbc:2           ','P 42/n b c ','-P 4ac 2b        ','        '),       &
 sg_info_type(134,'1    ','P42/nnm:1           ','P 42/n n m ',' P 4n 2 -1n      ','        '),       &
 sg_info_type(134,'2    ','P42/nnm:2           ','P 42/n n m ','-P 4ac 2bc       ','        '),       &
 sg_info_type(135,'     ','P42/mbc             ','P 42/m b c ','-P 4c 2ab        ','        '),       &
 sg_info_type(136,'     ','P42/mnm             ','P 42/m n m ','-P 4n 2n         ','        '),       &
 sg_info_type(137,'1    ','P42/nmc:1           ','P 42/n m c ',' P 4n 2n -1n     ','        '),       &
 sg_info_type(137,'2    ','P42/nmc:2           ','P 42/n m c ','-P 4ac 2a        ','        '),       &
 sg_info_type(138,'1    ','P42/ncm:1           ','P 42/n c m ',' P 4n 2ab -1n    ','        '),       &
 sg_info_type(138,'2    ','P42/ncm:2           ','P 42/n c m ','-P 4ac 2ac       ','        '),       &
 sg_info_type(139,'     ','I4/mmm              ','I 4/m m m  ','-I 4 2           ','        '),       &
 sg_info_type(140,'     ','I4/mcm              ','I 4/m c m  ','-I 4 2c          ','        '),       &
 sg_info_type(141,'1    ','I41/amd:1           ','I 41/a m d ',' I 4bw 2bw -1bw  ','        '),       &
 sg_info_type(141,'2    ','I41/amd:2           ','I 41/a m d ','-I 4bd 2         ','        '),       &
 sg_info_type(142,'1    ','I41/acd:1           ','I 41/a c d ',' I 4bw 2aw -1bw  ','        '),       &
 sg_info_type(142,'2    ','I41/acd:2           ','I 41/a c d ','-I 4bd 2c        ','        '),       &
 sg_info_type(143,'     ','P3                  ','P 3        ',' P 3             ','        '),       &
 sg_info_type(144,'     ','P31                 ','P 31       ',' P 31            ','        '),       &
 sg_info_type(145,'     ','P32                 ','P 32       ',' P 32            ','        '),       &
 sg_info_type(146,'h    ','R3:h                ','R 3        ',' R 3             ','        '),       &
 sg_info_type(146,'r    ','R3:r                ','R 3        ',' P 3*            ','        '),       &
 sg_info_type(147,'     ','P-3                 ','P -3       ','-P 3             ','        '),       &
 sg_info_type(148,'h    ','R-3:h               ','R -3       ','-R 3             ','        '),       &
 sg_info_type(148,'r    ','R-3:r               ','R -3       ','-P 3*            ','        '),       &
 sg_info_type(149,'     ','P312                ','P 3 1 2    ',' P 3 2           ','        '),       &
 sg_info_type(150,'     ','P321                ','P 3 2 1    ',' P 3 2"          ','        '),       &
 sg_info_type(151,'     ','P3112               ','P 31 1 2   ',' P 31 2c (0 0 1) ','        '),       &
 sg_info_type(152,'     ','P3121               ','P 31 2 1   ',' P 31 2"         ','        '),       &
 sg_info_type(153,'     ','P3212               ','P 32 1 2   ',' P 32 2c (0 0 -1)','        '),       &
 sg_info_type(154,'     ','P3221               ','P 32 2 1   ',' P 32 2"         ','        '),       &
 sg_info_type(155,'h    ','R32:h               ','R 3 2      ',' R 3 2"          ','        '),       &
 sg_info_type(155,'r    ','R32:r               ','R 3 2      ',' P 3* 2          ','        '),       &
 sg_info_type(156,'     ','P3m1                ','P 3 m 1    ',' P 3 -2"         ','        '),       &
 sg_info_type(157,'     ','P31m                ','P 3 1 m    ',' P 3 -2          ','        '),       &
 sg_info_type(158,'     ','P3c1                ','P 3 c 1    ',' P 3 -2"c        ','        '),       &
 sg_info_type(159,'     ','P31c                ','P 3 1 c    ',' P 3 -2c         ','        '),       &
 sg_info_type(160,'h    ','R3m:h               ','R 3 m      ',' R 3 -2"         ','        '),       &
 sg_info_type(160,'r    ','R3m:r               ','R 3 m      ',' P 3* -2         ','        '),       &
 sg_info_type(161,'h    ','R3c:h               ','R 3 c      ',' R 3 -2"c        ','        '),       &
 sg_info_type(161,'r    ','R3c:r               ','R 3 c      ',' P 3* -2n        ','        '),       &
 sg_info_type(162,'     ','P-31m               ','P -3 1 m   ','-P 3 2           ','        '),       &
 sg_info_type(163,'     ','P-31c               ','P -3 1 c   ','-P 3 2c          ','        '),       &
 sg_info_type(164,'     ','P-3m1               ','P -3 m 1   ','-P 3 2"          ','        '),       &
 sg_info_type(165,'     ','P-3c1               ','P -3 c 1   ','-P 3 2"c         ','        '),       &
 sg_info_type(166,'h    ','R-3m:h              ','R -3 m     ','-R 3 2"          ','        '),       &
 sg_info_type(166,'r    ','R-3m:r              ','R -3 m     ','-P 3* 2          ','        '),       &
 sg_info_type(167,'h    ','R-3c:h              ','R -3 c     ','-R 3 2"c         ','        '),       &
 sg_info_type(167,'r    ','R-3c:r              ','R -3 c     ','-P 3* 2n         ','        '),       &
 sg_info_type(168,'     ','P6                  ','P 6        ',' P 6             ','        '),       &
 sg_info_type(169,'     ','P61                 ','P 61       ',' P 61            ','        '),       &
 sg_info_type(170,'     ','P65                 ','P 65       ',' P 65            ','        '),       &
 sg_info_type(171,'     ','P62                 ','P 62       ',' P 62            ','        '),       &
 sg_info_type(172,'     ','P64                 ','P 64       ',' P 64            ','        '),       &
 sg_info_type(173,'     ','P63                 ','P 63       ',' P 6c            ','        '),       &
 sg_info_type(174,'     ','P-6                 ','P -6       ',' P -6            ','        '),       &
 sg_info_type(175,'     ','P6/m                ','P 6/m      ','-P 6             ','        '),       &
 sg_info_type(176,'     ','P63/m               ','P 63/m     ','-P 6c            ','        '),       &
 sg_info_type(177,'     ','P622                ','P 6 2 2    ',' P 6 2           ','        '),       &
 sg_info_type(178,'     ','P6122               ','P 61 2 2   ',' P 61 2 (0 0 -1) ','        '),       &
 sg_info_type(179,'     ','P6522               ','P 65 2 2   ',' P 65 2 (0 0 1)  ','        '),       &
 sg_info_type(180,'     ','P6222               ','P 62 2 2   ',' P 62 2c (0 0 1) ','        '),       &
 sg_info_type(181,'     ','P6422               ','P 64 2 2   ',' P 64 2c (0 0 -1)','        '),       &
 sg_info_type(182,'     ','P6322               ','P 63 2 2   ',' P 6c 2c         ','        '),       &
 sg_info_type(183,'     ','P6mm                ','P 6 m m    ',' P 6 -2          ','        '),       &
 sg_info_type(184,'     ','P6cc                ','P 6 c c    ',' P 6 -2c         ','        '),       &
 sg_info_type(185,'     ','P63cm               ','P 63 c m   ',' P 6c -2         ','        '),       &
 sg_info_type(186,'     ','P63mc               ','P 63 m c   ',' P 6c -2c        ','        '),       &
 sg_info_type(187,'     ','P-6m2               ','P -6 m 2   ',' P -6 2          ','        '),       &
 sg_info_type(188,'     ','P-6c2               ','P -6 c 2   ',' P -6c 2         ','        '),       &
 sg_info_type(189,'     ','P-62m               ','P -6 2 m   ',' P -6 -2         ','        '),       &
 sg_info_type(190,'     ','P-62c               ','P -6 2 c   ',' P -6c -2c       ','        '),       &
 sg_info_type(191,'     ','P6/mmm              ','P 6/m m m  ','-P 6 2           ','        '),       &
 sg_info_type(192,'     ','P6/mcc              ','P 6/m c c  ','-P 6 2c          ','        '),       &
 sg_info_type(193,'     ','P63/mcm             ','P 63/m c m ','-P 6c 2          ','        '),       &
 sg_info_type(194,'     ','P63/mmc             ','P 63/m m c ','-P 6c 2c         ','        '),       &
 sg_info_type(195,'     ','P23                 ','P 2 3      ',' P 2 2 3         ','        '),       &
 sg_info_type(196,'     ','F23                 ','F 2 3      ',' F 2 2 3         ','        '),       &
 sg_info_type(197,'     ','I23                 ','I 2 3      ',' I 2 2 3         ','        '),       &
 sg_info_type(198,'     ','P213                ','P 21 3     ',' P 2ac 2ab 3     ','        '),       &
 sg_info_type(199,'     ','I213                ','I 21 3     ',' I 2b 2c 3       ','        '),       &
 sg_info_type(200,'     ','Pm-3                ','P m -3     ','-P 2 2 3         ','        '),       &
 sg_info_type(201,'1    ','Pn-3:1              ','P n -3     ',' P 2 2 3 -1n     ','        '),       &
 sg_info_type(201,'2    ','Pn-3:2              ','P n -3     ','-P 2ab 2bc 3     ','        '),       &
 sg_info_type(202,'     ','Fm-3                ','F m -3     ','-F 2 2 3         ','        '),       &
 sg_info_type(203,'1    ','Fd-3:1              ','F d -3     ',' F 2 2 3 -1d     ','        '),       &
 sg_info_type(203,'2    ','Fd-3:2              ','F d -3     ','-F 2uv 2vw 3     ','        '),       &
 sg_info_type(204,'     ','Im-3                ','I m -3     ','-I 2 2 3         ','        '),       &
 sg_info_type(205,'     ','Pa-3                ','P a -3     ','-P 2ac 2ab 3     ','        '),       &
 sg_info_type(206,'     ','Ia-3                ','I a -3     ','-I 2b 2c 3       ','        '),       &
 sg_info_type(207,'     ','P432                ','P 4 3 2    ',' P 4 2 3         ','        '),       &
 sg_info_type(208,'     ','P4232               ','P 42 3 2   ',' P 4n 2 3        ','        '),       &
 sg_info_type(209,'     ','F432                ','F 4 3 2    ',' F 4 2 3         ','        '),       &
 sg_info_type(210,'     ','F4132               ','F 41 3 2   ',' F 4d 2 3        ','        '),       &
 sg_info_type(211,'     ','I432                ','I 4 3 2    ',' I 4 2 3         ','        '),       &
 sg_info_type(212,'     ','P4332               ','P 43 3 2   ',' P 4acd 2ab 3    ','        '),       &
 sg_info_type(213,'     ','P4132               ','P 41 3 2   ',' P 4bd 2ab 3     ','        '),       &
 sg_info_type(214,'     ','I4132               ','I 41 3 2   ',' I 4bd 2c 3      ','        '),       &
 sg_info_type(215,'     ','P-43m               ','P -4 3 m   ',' P -4 2 3        ','        '),       &
 sg_info_type(216,'     ','F-43m               ','F -4 3 m   ',' F -4 2 3        ','        '),       &
 sg_info_type(217,'     ','I-43m               ','I -4 3 m   ',' I -4 2 3        ','        '),       &
 sg_info_type(218,'     ','P-43n               ','P -4 3 n   ',' P -4n 2 3       ','        '),       &
 sg_info_type(219,'     ','F-43c               ','F -4 3 c   ',' F -4c 2 3       ','        '),       &
 sg_info_type(220,'     ','I-43d               ','I -4 3 d   ',' I -4bd 2c 3     ','        '),       &
 sg_info_type(221,'     ','Pm-3m               ','P m -3 m   ','-P 4 2 3         ','        '),       &
 sg_info_type(222,'1    ','Pn-3n:1             ','P n -3 n   ',' P 4 2 3 -1n     ','        '),       &
 sg_info_type(222,'2    ','Pn-3n:2             ','P n -3 n   ','-P 4a 2bc 3      ','        '),       &
 sg_info_type(223,'     ','Pm-3n               ','P m -3 n   ','-P 4n 2 3        ','        '),       &
 sg_info_type(224,'1    ','Pn-3m:1             ','P n -3 m   ',' P 4n 2 3 -1n    ','        '),       &
 sg_info_type(224,'2    ','Pn-3m:2             ','P n -3 m   ','-P 4bc 2bc 3     ','        '),       &
 sg_info_type(225,'     ','Fm-3m               ','F m -3 m   ','-F 4 2 3         ','        '),       &
 sg_info_type(226,'     ','Fm-3c               ','F m -3 c   ','-F 4c 2 3        ','        '),       &
 sg_info_type(227,'1    ','Fd-3m:1             ','F d -3 m   ',' F 4d 2 3 -1d    ','        '),       &
 sg_info_type(227,'2    ','Fd-3m:2             ','F d -3 m   ','-F 4vw 2vw 3     ','        '),       &
 sg_info_type(228,'1    ','Fd-3c:1             ','F d -3 c   ',' F 4d 2 3 -1cd   ','        '),       &
 sg_info_type(228,'2    ','Fd-3c:2             ','F d -3 c   ','-F 4cvw 2vw 3    ','        '),       &
 sg_info_type(229,'     ','Im-3m               ','I m -3 m   ','-I 4 2 3         ','        '),       &
 sg_info_type(230,'     ','Ia-3d               ','I a -3 d   ','-I 4bd 2c 3      ','        ')        &
/) 


integer, private :: i
type(extsymb_t), dimension(217) :: extsy = [  &
   extsymb_t('P - 1 1',3, [5,20,59,(0,i=4,16)]),  &
   extsymb_t('P 21 1 1',2, [8,62,(0,i=3,16)]),  &
   extsymb_t('P b 1 1',2, [27,78,(0,i=3,16)]),  &
   extsymb_t('P 21/b 1 1',1, [87,(0,i=2,16)]),  &
   extsymb_t('P c 1 1',2, [29,80,(0,i=3,16)]),  &
   extsymb_t('P 21/c 1 1',1, [89,(0,i=2,16)]),  &
   extsymb_t('P n 1 1',2, [28,79,(0,i=3,16)]),  &
   extsymb_t('P 21/n 1 1',1, [88,(0,i=2,16)]),  &
   extsymb_t('C - 1 1',3, [16,37,70,(0,i=4,16)]),  &
   extsymb_t('C n 1 1',2, [52,103,(0,i=3,16)]),  &
   extsymb_t('B - 1 1',3, [15,36,69,(0,i=4,16)]),  &
   extsymb_t('B b 1 1',2, [51,102,(0,i=3,16)]),  &
   extsymb_t('I - 1 1',3, [17,38,71,(0,i=4,16)]),  &
   extsymb_t('I c 1 1',2, [53,104,(0,i=3,16)]),  &
   extsymb_t('P 1 - 1',3, [3,18,57,(0,i=4,16)]),  &
   extsymb_t('P 1 21 1',2, [6,60,(0,i=3,16)]),  &
   extsymb_t('P 1 a 1',2, [23,74,(0,i=3,16)]),  &
   extsymb_t('P 1 21/a 1',1, [83,(0,i=2,16)]),  &
   extsymb_t('P 1 c 1',2, [21,72,(0,i=3,16)]),  &
   extsymb_t('P 1 21/c 1',1, [81,(0,i=2,16)]),  &
   extsymb_t('P 1 n 1',2, [22,73,(0,i=3,16)]),  &
   extsymb_t('P 1 21/n 1',1, [82,(0,i=2,16)]),  &
   extsymb_t('C 1 - 1',3, [9,30,63,(0,i=4,16)]),  &
   extsymb_t('C 1 c 1',2, [39,90,(0,i=3,16)]),  &
   extsymb_t('A 1 - 1',3, [10,31,64,(0,i=4,16)]),  &
   extsymb_t('A 1 n 1',2, [40,91,(0,i=3,16)]),  &
   extsymb_t('I 1 - 1',3, [11,32,65,(0,i=4,16)]),  &
   extsymb_t('I 1 a 1',2, [41,92,(0,i=3,16)]),  &
   extsymb_t('P 1 1 -',3, [4,19,58,(0,i=4,16)]),  &
   extsymb_t('P 1 1 21',2, [7,61,(0,i=3,16)]),  &
   extsymb_t('P 1 1 a',2, [24,75,(0,i=3,16)]),  &
   extsymb_t('P 1 1 21/a',1, [84,(0,i=2,16)]),  &
   extsymb_t('P 1 1 b',2, [26,77,(0,i=3,16)]),  &
   extsymb_t('P 1 1 21/b',1, [86,(0,i=2,16)]),  &
   extsymb_t('P 1 1 n',2, [25,76,(0,i=3,16)]),  &
   extsymb_t('P 1 1 21/n',1, [85,(0,i=2,16)]),  &
   extsymb_t('B 1 1 -',3, [13,34,67,(0,i=4,16)]),  &
   extsymb_t('B 1 1 n',2, [46,97,(0,i=3,16)]),  &
   extsymb_t('A 1 1 -',3, [12,33,66,(0,i=4,16)]),  &
   extsymb_t('A 1 1 a',2, [45,96,(0,i=3,16)]),  &
   extsymb_t('I 1 1 -',3, [14,35,68,(0,i=4,16)]),  &
   extsymb_t('I 1 1 b',2, [47,98,(0,i=3,16)]),  &
   extsymb_t('P - - -',5, [108,125,227,127,126,(0,i=6,16)]),  &
   extsymb_t('P - - 21',1, [109,(0,i=2,16)]),  &
   extsymb_t('P - 21 -',1, [111,(0,i=2,16)]),  &
   extsymb_t('P - 21 21',1, [113,(0,i=2,16)]),  &
   extsymb_t('P 21 - -',1, [110,(0,i=2,16)]),  &
   extsymb_t('P 21 - 21',1, [114,(0,i=2,16)]),  &
   extsymb_t('P 21 21 -',1, [112,(0,i=2,16)]),  &
   extsymb_t('P 21 21 21',1, [115,(0,i=2,16)]),  &
   extsymb_t('P - - a',3, [142,130,239,(0,i=4,16)]),  &
   extsymb_t('P - - b',3, [133,139,240,(0,i=4,16)]),  &
   extsymb_t('P - - n',3, [160,157,279,(0,i=4,16)]),  &
   extsymb_t('P - a -',3, [137,244,131,(0,i=4,16)]),  &
   extsymb_t('P - a a',2, [135,231,(0,i=3,16)]),  &
   extsymb_t('P - a b',2, [145,272,(0,i=3,16)]),  &
   extsymb_t('P - a n',2, [152,256,(0,i=3,16)]),  &
   extsymb_t('P - c -',3, [128,140,243,(0,i=4,16)]),  &
   extsymb_t('P - c a',2, [146,271,(0,i=3,16)]),  &
   extsymb_t('P - c b',2, [162,264,(0,i=3,16)]),  &
   extsymb_t('P - c n',2, [167,296,(0,i=3,16)]),  &
   extsymb_t('P - n -',3, [155,158,283,(0,i=4,16)]),  &
   extsymb_t('P - n a',2, [151,251,(0,i=3,16)]),  &
   extsymb_t('P - n b',2, [166,293,(0,i=3,16)]),  &
   extsymb_t('P - n n',2, [171,276,(0,i=3,16)]),  &
   extsymb_t('P b - -',3, [138,132,241,(0,i=4,16)]),  &
   extsymb_t('P b - a',2, [148,273,(0,i=3,16)]),  &
   extsymb_t('P b - b',2, [136,232,(0,i=3,16)]),  &
   extsymb_t('P b - n',2, [153,253,(0,i=3,16)]),  &
   extsymb_t('P b a -',2, [161,263,(0,i=3,16)]),  &
   extsymb_t('P b a a',1, [259,(0,i=2,16)]),  &
   extsymb_t('P b a b',1, [262,(0,i=2,16)]),  &
   extsymb_t('P b a n',1, [234,(0,i=2,16)]),  &
   extsymb_t('P b c -',2, [144,269,(0,i=3,16)]),  &
   extsymb_t('P b c a',1, [290,(0,i=2,16)]),  &
   extsymb_t('P b c b',1, [261,(0,i=2,16)]),  &
   extsymb_t('P b c n',1, [284,(0,i=2,16)]),  &
   extsymb_t('P b n -',2, [165,294,(0,i=3,16)]),  &
   extsymb_t('P b n a',1, [288,(0,i=2,16)]),  &
   extsymb_t('P b n b',1, [268,(0,i=2,16)]),  &
   extsymb_t('P b n n',1, [247,(0,i=2,16)]),  &
   extsymb_t('P c - -',3, [129,141,242,(0,i=4,16)]),  &
   extsymb_t('P c - a',2, [163,265,(0,i=3,16)]),  &
   extsymb_t('P c - b',2, [147,274,(0,i=3,16)]),  &
   extsymb_t('P c - n',2, [168,295,(0,i=3,16)]),  &
   extsymb_t('P c a -',2, [143,270,(0,i=3,16)]),  &
   extsymb_t('P c a a',1, [260,(0,i=2,16)]),  &
   extsymb_t('P c a b',1, [291,(0,i=2,16)]),  &
   extsymb_t('P c a n',1, [285,(0,i=2,16)]),  &
   extsymb_t('P c c -',2, [134,230,(0,i=3,16)]),  &
   extsymb_t('P c c a',1, [257,(0,i=2,16)]),  &
   extsymb_t('P c c b',1, [258,(0,i=2,16)]),  &
   extsymb_t('P c c n',1, [266,(0,i=2,16)]),  &
   extsymb_t('P c n -',2, [150,254,(0,i=3,16)]),  &
   extsymb_t('P c n a',1, [238,(0,i=2,16)]),  &
   extsymb_t('P c n b',1, [289,(0,i=2,16)]),  &
   extsymb_t('P c n n',1, [248,(0,i=2,16)]),  &
   extsymb_t('P n - -',3, [156,281,159,(0,i=4,16)]),  &
   extsymb_t('P n - a',2, [169,292,(0,i=3,16)]),  &
   extsymb_t('P n - b',2, [154,252,(0,i=3,16)]),  &
   extsymb_t('P n - n',2, [172,277,(0,i=3,16)]),  &
   extsymb_t('P n a -',2, [164,297,(0,i=3,16)]),  &
   extsymb_t('P n a a',1, [267,(0,i=2,16)]),  &
   extsymb_t('P n a b',1, [287,(0,i=2,16)]),  &
   extsymb_t('P n a n',1, [250,(0,i=2,16)]),  &
   extsymb_t('P n c -',2, [149,255,(0,i=3,16)]),  &
   extsymb_t('P n c a',1, [286,(0,i=2,16)]),  &
   extsymb_t('P n c b',1, [236,(0,i=2,16)]),  &
   extsymb_t('P n c n',1, [249,(0,i=2,16)]),  &
   extsymb_t('P n n -',2, [170,275,(0,i=3,16)]),  &
   extsymb_t('P n n a',1, [245,(0,i=2,16)]),  &
   extsymb_t('P n n b',1, [246,(0,i=2,16)]),  &
   extsymb_t('P n n n',1, [229,(0,i=2,16)]),  &
   extsymb_t('C - - -',5, [119,173,310,189,188,(0,i=6,16)]),  &
   extsymb_t('C - - 21',1, [116,(0,i=2,16)]),  &
   extsymb_t('C - - (ab)',4, [195,316,194,317,(0,i=5,16)]),  &
   extsymb_t('C - c -',3, [176,298,200,(0,i=4,16)]),  &
   extsymb_t('C - c (ab)',2, [206,304,(0,i=3,16)]),  &
   extsymb_t('C c - -',3, [177,299,201,(0,i=4,16)]),  &
   extsymb_t('C c - (ab)',2, [207,305,(0,i=3,16)]),  &
   extsymb_t('C c c -',2, [182,313,(0,i=3,16)]),  &
   extsymb_t('C c c (ab)',2, [323,325,(0,i=3,16)]),  &
   extsymb_t('B - - -',5, [121,186,312,175,187,(0,i=6,16)]),  &
   extsymb_t('B - 21 -',1, [118,(0,i=2,16)]),  &
   extsymb_t('B - - b',3, [181,303,199,(0,i=4,16)]),  &
   extsymb_t('B - (ac)-',4, [192,321,193,320,(0,i=5,16)]),  &
   extsymb_t('B - (ac)b',2, [205,309,(0,i=3,16)]),  &
   extsymb_t('B b - -',3, [198,302,180,(0,i=4,16)]),  &
   extsymb_t('B b - b',2, [184,315,(0,i=3,16)]),  &
   extsymb_t('B b (ac)-',2, [204,308,(0,i=3,16)]),  &
   extsymb_t('B b (ac)b',2, [333,331,(0,i=3,16)]),  &
   extsymb_t('A - - -',5, [120,185,311,190,174,(0,i=6,16)]),  &
   extsymb_t('A 21 - -',1, [117,(0,i=2,16)]),  &
   extsymb_t('A - - a',3, [202,300,178,(0,i=4,16)]),  &
   extsymb_t('A - a -',3, [197,301,179,(0,i=4,16)]),  &
   extsymb_t('A - a a',2, [183,314,(0,i=3,16)]),  &
   extsymb_t('A(bc)- -',4, [191,318,196,319,(0,i=5,16)]),  &
   extsymb_t('A(bc)- a',2, [208,306,(0,i=3,16)]),  &
   extsymb_t('A(bc)a -',2, [203,307,(0,i=3,16)]),  &
   extsymb_t('A(bc)a a',2, [327,329,(0,i=3,16)]),  &
   extsymb_t('I - - -',6, [123,215,337,124,217,216,(0,i=7,16)]),  &
   extsymb_t('I - - (ab)',4, [226,343,223,344,(0,i=5,16)]),  &
   extsymb_t('I - (ac)-',4, [221,348,224,347,(0,i=5,16)]),  &
   extsymb_t('I - c b',2, [219,339,(0,i=3,16)]),  &
   extsymb_t('I(bc)- -',4, [222,345,225,346,(0,i=5,16)]),  &
   extsymb_t('I c - a',2, [220,340,(0,i=3,16)]),  &
   extsymb_t('I b a -',2, [218,338,(0,i=3,16)]),  &
   extsymb_t('I b c a',1, [341,(0,i=2,16)]),  &
   extsymb_t('F - - -',5, [122,209,334,211,210,(0,i=6,16)]),  &
   extsymb_t('F - d d',1, [213,(0,i=2,16)]),  &
   extsymb_t('F d - d',1, [214,(0,i=2,16)]),  &
   extsymb_t('F d d -',1, [212,(0,i=2,16)]),  &
   extsymb_t('F d d d',1, [336,(0,i=2,16)]),  &
   extsymb_t('P - - -',8, [349,355,357,366,376,388,400,392,(0,i=9,16)]),  &
   extsymb_t('P - 21 -',2, [367,390,(0,i=3,16)]),  &
   extsymb_t('P 42 - -',3, [351,358,370,(0,i=4,16)]),  &
   extsymb_t('P 42 21 -',1, [371,(0,i=2,16)]),  &
   extsymb_t('P 41 - -',4, [350,352,368,372,(0,i=5,16)]),  &
   extsymb_t('P 41 21 -',2, [369,373,(0,i=3,16)]),  &
   extsymb_t('P - - c',3, [382,389,412,(0,i=4,16)]),  &
   extsymb_t('P - 21 c',1, [391,(0,i=2,16)]),  &
   extsymb_t('P - b -',3, [377,394,406,(0,i=4,16)]),  &
   extsymb_t('P - b c',2, [383,418,(0,i=3,16)]),  &
   extsymb_t('P - c -',3, [378,393,413,(0,i=4,16)]),  &
   extsymb_t('P - c c',2, [380,401,(0,i=3,16)]),  &
   extsymb_t('P - n -',3, [379,395,419,(0,i=4,16)]),  &
   extsymb_t('P - n c',2, [381,407,(0,i=3,16)]),  &
   extsymb_t('P n - -',2, [360,409,(0,i=3,16)]),  &
   extsymb_t('P 42/n - -',1, [362,(0,i=2,16)]),  &
   extsymb_t('P n - c',1, [421,(0,i=2,16)]),  &
   extsymb_t('P n b -',1, [403,(0,i=2,16)]),  &
   extsymb_t('P n b c',1, [415,(0,i=2,16)]),  &
   extsymb_t('P n c -',1, [423,(0,i=2,16)]),  &
   extsymb_t('P n c c',1, [411,(0,i=2,16)]),  &
   extsymb_t('P n n -',1, [417,(0,i=2,16)]),  &
   extsymb_t('P n n c',1, [405,(0,i=2,16)]),  &
   extsymb_t('I - - -',8, [353,356,363,374,384,398,424,396,(0,i=9,16)]),  &
   extsymb_t('I 41 - -',2, [354,375,(0,i=3,16)]),  &
   extsymb_t('I - - d',2, [386,399,(0,i=3,16)]),  &
   extsymb_t('I - c -',3, [385,397,425,(0,i=4,16)]),  &
   extsymb_t('I - c d',1, [387,(0,i=2,16)]),  &
   extsymb_t('I 41/a - -',1, [365,(0,i=2,16)]),  &
   extsymb_t('I a - d',1, [427,(0,i=2,16)]),  &
   extsymb_t('I a c d',1, [429,(0,i=2,16)]),  &
   extsymb_t('P - - -',16, [430,435,439,446,456,438,447,454,462,468,469,471,477,483,485,481]),   &
   extsymb_t('P 31 - -',6, [431,441,440,432,443,442,(0,i=7,16)]),  &
   extsymb_t('P - - c',5, [449,455,480,484,488,(0,i=6,16)]),  &
   extsymb_t('P - c -',5, [448,457,479,482,487,(0,i=6,16)]),  &
   extsymb_t('R (obv) - -',5, [434,437,445,451,459,(0,i=6,16)]),  &
   extsymb_t('R (obv)- - c',2, [453,461,(0,i=3,16)]),  &
   extsymb_t('R (rev) - -',5, [434,437,445,451,459,(0,i=6,16)]),  &
   extsymb_t('R (rev)- - c',2, [453,461,(0,i=3,16)]),  &
   extsymb_t('R - -',5, [434,437,445,451,459,(0,i=6,16)]),  &
   extsymb_t('R - c',2, [453,461,(0,i=3,16)]),  &
   extsymb_t('P 63 - -',3, [467,470,476,(0,i=4,16)]),  &
   extsymb_t('P 62 - -',4, [465,474,466,475,(0,i=5,16)]),  &
   extsymb_t('P 61 - -',4, [463,472,464,473,(0,i=5,16)]),  &
   extsymb_t('P - - c',2, [478,486,(0,i=3,16)]),  &
   extsymb_t('P - c c',5, [489,494,503,511,517,(0,i=6,16)]),  &
   extsymb_t('P - - -',1, [492,(0,i=2,16)]),  &
   extsymb_t('P 21(42) - -',1, [504,(0,i=2,16)]),  &
   extsymb_t('P 41 - -',2, [509,508,(0,i=3,16)]),  &
   extsymb_t('P - - n',2, [514,520,(0,i=3,16)]),  &
   extsymb_t('P a - -',1, [501,(0,i=2,16)]),  &
   extsymb_t('P n - -',2, [496,522,(0,i=3,16)]),  &
   extsymb_t('P n - n',1, [519,(0,i=2,16)]),  &
   extsymb_t('I - - -',6, [491,493,500,507,513,529,(0,i=7,16)]),  &
   extsymb_t('I 41 - -',1, [510,(0,i=2,16)]),  &
   extsymb_t('I - - d',1, [516,(0,i=2,16)]),  &
   extsymb_t('I a - -',1, [502,(0,i=2,16)]),  &
   extsymb_t('I a - d',1, [530,(0,i=2,16)]),  &
   extsymb_t('F - - -',5, [490,497,505,512,523,(0,i=6,16)]),  &
   extsymb_t('F 41 - -',1, [506,(0,i=2,16)]),  &
   extsymb_t('F - - c',2, [515,524,(0,i=3,16)]),  &
   extsymb_t('F d - -',2, [499,526,(0,i=3,16)]),  &
   extsymb_t('F d - c',1, [528,(0,i=2,16)]),  &
   extsymb_t('P -',2, [1,2,(0,i=3,16)])  &
   ]
contains 

   integer function get_index_exts(sgindex) result(iexts)
!
!  Convert index in array sg_info in index in array extsymb_t
!
   integer, intent(in) :: sgindex
   integer             :: i,j
!
   iexts = 0
   do i=1,size(extsy)
      do j=1,extsy(i)%nspg
         if (sgindex == extsy(i)%spgn(j)) then
             iexts = i
         endif
      enddo
   enddo
!
   end function get_index_exts

!-----------------------------------------------------------------------------

  function get_hm1(sg)
  type(sg_info_type), intent(in) :: sg
  integer                        :: pos
  character(len=LEN_HM)          :: get_hm1
!
  pos = index(sg%hm1,'=')
  if (pos == 0) then
      get_hm1 = sg%hm1
  else
      get_hm1 = sg%hm1(pos+2:)
  endif
!
  end function get_hm1

END MODULE symm_table
