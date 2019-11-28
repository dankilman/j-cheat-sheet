NB. ---------------------------------------------------------
NB. Assignment
NB. ---------------------------------------------------------
x=: y                    NB. global assign value of y to x
x=. y                    NB. local assign value of y to x
'a b c'=: n m w          NB. [a =: n, b =: m, c =: w]
('a';'b';'c')=: n m w    NB. "
'a b c'=: n;m;w          NB. "
('a';'b';'c')=: n;m;w    NB. "

NB. ---------------------------------------------------------
NB. Logic
NB. ---------------------------------------------------------
x *. y                   NB. x and y
x +. y                   NB. x or y
-. y                     NB. not y
x *: y                   NB. not (x and y)
x +: y                   NB. not (x or y)
x -: y                   NB. x matches y (same shape and values: identical, atomic result)
x = y                    NB. x equals y (same shape collection of item comparisons)
x >: y                   NB. x >= y
x <: y                   NB. x <= y

NB. ---------------------------------------------------------
NB. Number representation
NB. ---------------------------------------------------------
Yx                       NB. Y number, literal x: extended integer (bigint)
XeY                      NB. X number, literal e, Y pos/neg integer: scientific notation [X*10^Y]
XrY                      NB. rational number [X / Y: both components are stored as extended integers]
XjY                      NB. complex number [real=X, imag=Y]
XarY                     NB. complex number [magnitude=x, angle-rad=y]
XadY                     NB. complex number [magnitude=x, angle-deg=y]
XpY                      NB. X * pi^Y
XxY                      NB. X * e^Y
NbDDD.DDD                NB. [N=number base, DDD.DDD=digits; a-z chars used for "digits" > 9]

NB. ---------------------------------------------------------
NB. Numerical Math
NB. ---------------------------------------------------------
_                        NB. infinity [float]
__                       NB. negative infinity [float]
_.                       NB. NaN [float]
| y                      NB. abs(y) [magnitude]
x | y                    NB. y modulo x
Xx % Yx                  =   XrY  NB. literal x and r
x: y                     NB. rational number from float y
x:^:_1 y                 NB. float from rational number
(2 x: XrY)               =   X Y
x j. y                   NB. complex number [real=x, imag=y]
x r. y                   NB. complex number [magnitude=x, angle-rad=y]
+. XjY                   =   x y
*. XjY                   =   (| x) 'ANGLE-RAD'
(128 !: 5) y             NB. is y _. (NaN)
x + y                    NB. add
+ XjY                    =   Xj_Y  NB. conjugate
x - y                    NB. subtract
- y                      NB. negate
x * y                    NB. multiply
* y                      NB. sign of y [unit angle vector for complex]
*: y                     =   y * y
x % y                    NB. divide [1%0=_, 0%0=0]
% y                      =   1 % y
+: y                     =   y * 2
-: y                     =   y % 2
x ^ y                    NB. pow(x, y)
^y                       NB. e^y
x %: y                   =   y ^ % x
%: y                     NB. sqrt(y)
x ^. y                   NB. log(y, base=x)
^. y                     NB. log(y, base=e)
x +. y                   NB. GCD
x *. y                   NB. LCM
x # y                    NB. create a list with x(i) occurrences for y(i) [#x = #y or #x = 1 where x(0) is repeated]
#. y                     NB. y binary list as number
#: y                     NB. y number as a binary list
x #. y                   NB. y base list as specified by spec x as a number
x #: y                   NB. y number to base list as specified by spec x
>. y                     NB. round up
<. y                     NB. round down
x >. y                   NB. max(x, y)
x <. y                   NB. min(x, y)
>: y                     =   y + 1
<: y                     =   y - 1
! y                      NB. factorial(y)
x ! y                    NB. # of combinations of x selected from y (n k)
? y                      NB. random integer in range [0, y-1]
? 0                      NB. random float in range [0, 1)
x ? y                    NB. randomly select x integers in range [0, y-1]
o. y                     NB. y * pi
n o. y                   NB. trigonometric and other functions determined by n applied on y
q: y                     NB. prime factors of y
x q: y                   NB. list of x first exponents of y prime factors [_ for full list]
p: y                     NB. get (y-1)'th prime
x p: y                   NB. various prime related functions [function determined by x]
y p. x                   =   +/ y * x ^ i. # y  NB. calculate polynomial specified by coefficients y on x
(m, roots) p. x          =   m * */(x - roots)  NB. calculate coefficients
(< C ,. E) p. x          NB. C,E pairs of a coefficient and exponent to get polynomial
NB.                          a polynomial function can be obtained with y&p.
p. y                     NB. 1. roots of polynomial specified by coefficients y [m; roots]
NB.                          2. coefficients of polynomial specified by roots y [m; roots]
NB.                          m such that: p(x) = m*(x-r1)*(x-r2)*...*(x-rn)
x d. n                   NB. n'th derivative of x [analytic]
x d. _1                  NB. integrate x [analytic]
x D. n                   NB. n'th derivative of x [numeric]

NB. ---------------------------------------------------------
NB. Matrices
NB. ---------------------------------------------------------
M1 (+/ . *) M2           NB. dot product (M1, M2 are vectors or matrices)
u . v                    =   (u @: v) " (1+L, _)  NB. L = 1 { (v b. 0) [left rank of v]
(- / . *) M              NB. determinant of matrix M
R %. M                   NB. find solutions U [M is a matrix, R is right hand size values] (M dot U = R)
M %. M                   NB. Identity matrix `I` such that [M dot I = M]
%. M                     NB. Inverse matrix [= I %. M where I is the identity matrix of the size of M]

NB. ---------------------------------------------------------
NB. Arrays
NB. ---------------------------------------------------------
x $ y                    NB. build table with shape x with values y
# y                      NB. len(y)
|. y                     NB. reverse y
x |. y                   NB. rotate y by x places [positive rotate to left, negative vice-versa]
NB.                          successive items in x rotate succesive axes
x (|. !. n) y            NB. shift (like rotate but edges are filled with n)
, y                      NB. assemble all elements into a flat list
x , y                    NB. join x and y lists [concatenate]
,. y                     NB. for each item assemble into flat list
x ,. y                   NB. for each item x1,y1
,: y                     NB. make into a 1-item array by adding a prefix dimension of 1
x ,: y                   NB. (x,y) with two items x and y
x { y                    NB. select x index from y (x has its own world of syntax)
x {. y                   NB. take x first items [abs(x) last items when negative]
{. y                     =   1 {. y
x }. y                   NB. drop x first items [abs(x) last items when negative]
}. y                     =   1 }. y
{: y                     NB. last item
}: y                     NB. all but last item
x } y                    NB. combine items of y according to the items x
x (n }) y                NB. amend (adverb) y with x at n (n has same index as select)
x (U`V`W) } y            =   (x U y) (x V y) } (x W y)
i.y                      NB. 0 1 2 ... (y-1)
x i. y                   NB. find first index of item y in list x
/: y                     NB. indices permutation that sorts y ascending [sorted(y) == (/: y) { y]
\: y                     NB. indices permutation that sorts y descending
x /: y                   =   (/: y) { x  NB. sort x by y ascending. [/:~y for basic sort, x can be a table for multiple sort keys]
x \: y                   =   (/: y) { x  NB. sort x by y descending
C. y                     NB. convert between direct and cyclic permutations [e.g. (4 2 3 1 0) <=> (3 1 2; 4 0)]
x C. y                   NB. apply permutation x on y [x may be either a direct or a cyclic representation]
NB.                          x may be abbreviated direct, in which case it means move selection to tail of y.
NB.                          x may be abbreviated cycle [box], in which case, the specified cycle is applied
(/: x) C. (x C. y)       =   y
A. y                     NB. anagram(permutation) index from anagram given permutation y
x A. y                   NB. anagram index x of list y
|: y                     NB. tranpose (reverse the order of the axes of an n-dimensional [rank-n] array)
x |: y                   NB. restructure y with axes listed in x as last [axes boxed together extract their common diagonal]
u ;. 0 y                 NB. apply u on whole of y after reversal along each axis
x u ;. 0 y               NB. apply u on y subarray according to spec x [x == (starting indices ,: ranges)]
NB.                          negating a range specifies reversal after positive selection along that axis
u ;. n y                 NB. apply u on sections of y split as specified by n:
NB.                          first item is boundary marker (fret) for abs(n) == 1
NB.                          last item is boundary marker (fret) for abs(n) == 2
NB.                          frets are retained in sections for n > 0, excluded for n < 0
x u ;. n y               NB. like the monad above but with x as a bitstring of which chars in y to consider fret markers
x u :. 3 y               NB. apply u on y tiles according to spec x [x == (starting-indices ,; ranges)]
NB.                          n == 3 includes incomplete parts (shards); n == _3 excludes them
;: y                     NB. split (parse) string y into boxed top-level J expression parts (tokens)

NB. ---------------------------------------------------------
NB. Sparse Arrays
NB. ---------------------------------------------------------
$. y                     NB. convert dense (regular) array y to sparse array
3 $. y                   NB. get sparse element of sparse array y
0 $. y                   NB. convert sparse array to dense (regular) array (and dense to sparse)
1 $. s;a;z               NB. create a sparse array [s=shape,a=axes,z=sparse value]
4 $. y                   NB. get index-matrix of sparse array y
5 $. y                   NB. get values of sparse array [index matches result of 4&$.]

NB. ---------------------------------------------------------
NB. Sets
NB. ---------------------------------------------------------
x e. y                   NB. true if x is a member of list y
x -. y                   NB. remove items from x that are in y, preserve order
~. y                     NB. remove duplicates (nub)
~: y                     NB. non duplicate sieve [bitstring, subsequent duplicate values yield false]
NB.                          (~: y) # y = ~. y
= y                      =   (~. y) =/ y

NB. ---------------------------------------------------------
NB. Trees
NB. ---------------------------------------------------------
{:: y                    NB. get paths to leaves of tree y
x {:: y                  NB. fetch value from tree y [e.g. x=(2;1;1)
L. y                     NB. max length of any path to a leaf
f L: n y                 NB. apply f on every n-th level of fixed level tree y [negative n for count from root]
NB.                          retain original structure of y
f S: n y                 NB. like L: but result is returned as a list

NB. ---------------------------------------------------------
NB. Box
NB. ---------------------------------------------------------
< y                      NB. box
> y                      NB. unbox
; y                      NB. unbox all elements into flat list
x ; y                    NB. list of x and then y [after adding a level of boxing to x, but not to y if already boxed]

NB. ---------------------------------------------------------
NB. Functions
NB. ---------------------------------------------------------
[ y                      NB. identity y
] y                      NB. identity y
x [ y                    NB. select x
x ] y                    NB. select y
n:                       NB. constant function with value of decimal digit n [_9,9, e.g. 2:]
y b.                     NB. y in [0-15]: return f such that f's truth table is y as a bitstring
NB.                          y in [16-31]: same as above but for integers bitwise instead of booleans
NB.                          y in [32-34]: more bitwise - see docs
f b. 0                   NB. ranks G, L, R of monadic f, left dyad f, right dyad f
f / y                    NB. i(1) f i(2) ... f i(n) [y=i(1) i(2) ... i(n)]
f`g`h / y                NB. i(1) f i(2) g i(3) h i(4) f ... [y=i(1) i(2) ...]
x f / y                  NB. a table of (x(i) f y(j)) for each x(i) in x and y(j) in y
NB.                          x rank should match left rank of f, y rank should match right rank of f
x f /. y                 =   (= x) (u @ #) y
x f~ y                   =   y f x  NB. argument transposition
f~ y                     =   y f y  NB. reflexive application
g f.                     NB. "fix" or "freeze" the function g [by eager evaluation]
f \ y                    NB. apply f to each prefix of items in y [(f y(1)) (f (y(1) y(2)) ...]
n f \ y                  NB. apply f to successive infixes of y of length n [negative n for no overlap]
f \. y                   NB. apply f to each suffix of items in y [from long to short]
n f \. y                 NB. apply f to successive outfix "views" of y where an infix of length n is removed [negative n for no overlap]
g ^: _1                  NB. inverse of g
f !. n                   NB. modify (fit or customize) f's behviour for certain specific functions
(f &. g) y               =   (g ^: _1) f g y
u each y                 =   &.>  NB. apply u inside each top-level box in y, and rebox
(f & k) y                =   y f k
(k & f) y                =   k f y
u every y                =   &>  NB. apply u inside each top-level box of y, and unbox
(f @: g) y               =   f (g y)
x (f @: g) y             =   f (x g y)
(f &: g) y               =   f (g y)
x (f &: g) y             =   (g x) f (g y)
(f @ g) y                =   (f @: g) " G  y NB. [G is the intrinsic rank of monadic g]
x (f @ g) y              =   x (f @: g) " LR y NB. [L,R are the left right intrisic ranks of g]
(f & g) y                =   (f @: g) " G y
x (f & g) y              =   (g x) (f " (G,G)) (g y)
(f g) y                  =   y f (g y)  NB. hook
x (f g) y                =   x f (g y)  NB. hook
(f g h) y                =   (f y) g (h y)  NB. fork
x (f g h) y              =   (x f y) g (x h y)  NB. fork
e f g h                  =   e (f g h)
d e f g h                =   d e (f g h)
NB.                          even count (a b c ...) =  hook: (a (b c ...))
NB.                          odd count (a b c ...) = fork: (a b (c ...))
n u v                    =   (n " _) u v NB. [noun-verb-verb]
(u " k y)                NB. the monadic verb u is applied separately to each k-cell of y
x (u " (L,R)) y          NB. apply dyad u separately to each pair consisting of an L-cell from x and the corresponding R-cell from y
u " _n y                 NB. u is to be applied to cells of rank n less than the rank of y
k " R                    =   (3 : 'k') " R  NB. [constant value k], [R=_ for always k, regardless of input rank]
(x u y)                  NB. for arguments x and y, if u is a dyad with ranks L and R,
NB.                          and the L-frame of x is f,g and the R-frame of y is f (supposing y to have the shorter frame)
NB.                          then (x u y) is computed as (x u (g& $)"R y)
[: f g                   =   f @: g
f`g`h                    NB. gerund of f, g and h [essentially, a list of atomic representations of f, g and h]
f`g`h`:6                 =   (f g h)
(1 { f`g`h)`:6           =   g
u0`u1`...`un @. t y      NB. u(t(y))(y)
u`v`w @. y               =   (y { u`v`w) `: 6
u`v`w`:0 y               =   (u y) , (v y) , (w y)
$:                       NB. self reference to function within function (recursion)
(f ^: n) y               NB. f f ... f [n applications] y
(f ^: _) y               NB. f f ... f y [multiple applications until fixed point, f(n-1) == f(n)]
(f ^: g) y               =   f ^: (g y) y
(f ^: g ^: _) y          NB. g is boolean [while g(y); y = f(y)] then return result y
(u ^: (v1`v2)) y         =   u ^: (v1 y) (v2 y) NB. "for loop" [v1(y) = #iterations, v2(y) = starting value]
x (u ^: n) y             =   ((x & u) ^: n) y
x (u ^: (U`V`W)) y       =   (((x U y) & u) ^: (x V y)) (x W y)
                         NB. BODY Operator definition:
NB.                          a quoted string, or 0 to be followed by successive lines ending with separate ")"
1 : BODY                 NB. adverb [u is the supplied verb]
2 : BODY                 NB. conjunction [u as left verb, v as right verb]
3 : BODY                 NB. monad [y as right arg] or ambivalent with : line in middle of BODY, monadic then dyadic
4 : BODY                 NB. dyad [x as left arg, y as right arg]
13 : BODY                NB. tacit verb [no free variables x or y] from explicit definition [BODY restricted to single line]
NB.                          A name in BODY with no assigned value is assumed to denote a verb.
noun                     =   0
adverb                   =   1
conjunction              =   2
verb                     =   3
monad                    =   3
dyad                     =   4
def                      =   0
define                   =   :0
adverb def BODY          =   1 : BODY
conjunction def BODY     =   2 : BODY
verb def BODY            =   3 : BODY
monad def BODY           =   3 : BODY
dyad def BODY            =   4 : BODY
verb define LF BODY LF ) NB. 3 : 0 : 0 multiline definition
x (C u)                  =   x C u  NB. C is a conjunction, result is an adverb
x (u C)                  =   u C x  NB. C is a conjunction, result is an adverb
x (A1 A2)                =   (x A1) A2  NB. A1 and A2 are adverbs, result is an adverb
                         NB. conjunction - long left scope short right scope,
NB.                          adverb -      long left scope
NB.                          verb -        long right scope

NB. ---------------------------------------------------------
NB. Misc
NB. ---------------------------------------------------------
_                        NB. infinity [float]
i. 0 0                   NB. null [by convention]
type 'N'                 NB. part of speech (a boxed word) of object named in quotes
datatype y               NB. data type (a boxed phrase) of y
s: y                     NB. convert boxed string(s) y into a symbol(s),
NB.                          or report their position(s) in the Global Symbol Table
". y                     NB. execute y as a J expression
7!:5 <'N'                NB. storage used by variable named N (sizeof)
x I. y                   NB. index of y in intervals ending with (ascending) values in x

NB. ---------------------------------------------------------
NB. Representation
NB. ---------------------------------------------------------
": y                     NB. convert noun y to string
x ": y                   NB. format noun y to string with x spec [x complex: real width, imag precision]
x 8 !: y                 NB. extended formatting of y determined by spec x
9!:6''                   NB. inspect global box drawing chars
9!:7 y                   NB. set global box drawing chars [#y = 11]
9!:3 y                   NB. set function global representation [2=box,5=lin,6=parens,4=tree,atom=1]
5!:n <'y'                NB. get function representation for noun y as new noun [n same as y value for 9!:3]
y 5!:0                   NB. inverse of atomic representation [y == 1 for 9!:3, restore original value]

NB. ---------------------------------------------------------
NB. Locales
NB. ---------------------------------------------------------
N_L_ =: value            NB. define N in locale L
N__y =: value            NB. define N in locale referenced by variable y [y is box containing the locale's name]
N =: value               NB. define N in current locale
(0 !: 0) < 'f.ijs'       NB. load script f.ijs into current locale
names ''                 NB. show all names in current locale
names_L_ ''              NB. show all names in locale L
nl ''                    NB. show all names in current locale as boxed list
y nl ''                  NB. show all names in current locale with prefix y as boxed list
erase < 'L'              NB. remove L definition from first locale containing it in path
coname ''                NB. show name of current locale
cocurrent 'L'            NB. set current locale to L
conl 0                   NB. list all locale names
conl 1                   NB. list ids of object locales
copath 'L'               NB. show locale path of locale L [L can also be boxed]
y copath 'L'             NB. set locale path for locale L [y is a list of boxed names, don't forget to put z last]
coerase 'L'              NB. destroy locale L

NB. ---------------------------------------------------------
NB. Object Oriented Programming
NB. ---------------------------------------------------------
coclass 'L'           NB. introduce a new class L
coinsert 'L'          NB. this class to be a child of L
conew 'L'             NB. new object of class L
codestroy_L_ ''       NB. destroy locale L [normally referenced from a method named 'destroy'; in class definitions]

NB. Steps to create and use a class
NB. ===============================
coclass 'ClsName'        NB. create class and switch to locale 'ClsName'
create =: verb def ''    NB. this is the constructor. any monadic verb will do
some_method =: +         NB. add some methods
destroy =: codestroy     NB. create destructor
cocurrent 'L'            NB. switch back to locale L (or any other)
obj =: x conew 'ClsName' NB. creates a new object based on 'ClsName' and calls create__obj x [obj = boxed id reference]
x some_method__obj y     NB. call (x some_method y) on obj
destroy__obj ''          NB. done with this. garbage cleanup

NB. Steps to inherit from a class
NB. =============================
NB. note: call parent methods with f. adverb when overriding to remain in current locale when applying method
coclass 'ClsName2'       NB. create class and switch to locale 'ClsName2'
coinsert 'ClsName'       NB. add 'ClsName' to 'ClsName2''s path (if not there already)
NB. ... (see above)

NB. ---------------------------------------------------------
NB. Scripts
NB. ---------------------------------------------------------
NB. notes:
NB. - scripts loaded with 'load' can have private (=.) assignments
NB.   that are local to the script
NB. - verbs using other verbs that are local to the script should
NB.   fix calls (f.)
jpath '~user'            NB. user dir
load 'f1.ijs'            NB. load 'f1.ijs', stop on error, no display
loadd 'f1.ijs'           NB. load 'f1.ijs', stop on error, display
require 'f1.ijs'         NB. load 'f1.ijs' only if not loaded already
0!:0 < 'f1.ijs'          NB. low-level load 'f1.ijs', stop on error, no display
0!:1 < 'f1.ijs'          NB. low-level load 'f1.ijs', stop on error, display
0!:10 < 'f1.ijs'         NB. low-level load 'f1.ijs', continue on error, no display
0!:11 < 'f1.ijs'         NB. low-level load 'f1.ijs', continue on error, display
4!:3''                   NB. all loaded scripts in current session [boxed list]
4!:4 < 'N'               NB. index in (4!:3) of script that loaded N

NB. ---------------------------------------------------------
NB. Binary Data (Serialization)
NB. ---------------------------------------------------------
3!:1 y                   NB. noun y to J specific binary representation
3!:2 y                   NB. J specific binary representation y to noun
2&(3!:5) y               NB. float y to 8 chars [y can be a list]
_2&(3!:5) y              NB. 8 chars y to float
1&(3!:5) y               NB. float y to 4 chars
_1&(3!:5) y              NB. 4 chars y to float
3&(3!:4) y               NB. integer y to 8 chars
_3&(3!:4) y              NB. 8 chars y to integer
2&(3!:4) y               NB. integer y to 4 chars
_2&(3!:4) y              NB. 4 chars y to integer
1&(3!:4) y               NB. integer y to 2 chars
_1&(3!:4) y              NB. 2 chars y to integer
1&(3!:4) y               NB. integer y to 2 chars, unsigned
0&(3!:4) y               NB. 2 chars y to integer, unsigned

NB. ---------------------------------------------------------
NB. Unicode
NB. ---------------------------------------------------------
u: y                     NB. string y to unicode
4&u: y                   NB. [0, 2^16) integer y to unicode [e.g. 16b03b1]
8&u: y                   NB. unicode y to utf-8
7&u: y                   NB. utf-8 y to unicodes
3&u: y                   NB. unicode code-points of y

NB. ---------------------------------------------------------
NB. Files
NB. ---------------------------------------------------------
s fwrite 'P'             NB. write string s to path P
fread 'P'                NB. read path P as string
s fappend 'P'            NB. append string into file at path P
fread 'P';B;L            NB. read path P starting at B, size L
s fwrites 'P'            NB. write text s to path P (platform-dependent line end)
freads 'P'               NB. read text from path P (platform-dependent line end)
fexist 'P'               NB. true if path P exists
ferase 'P'               NB. delete file at path P
x 1!:2 <'P'              NB. low-level write x into path P
1!:1 <'P'                NB. low-level read path P as string
smoutput y               NB. write to stdout
x (1!:2) 2               NB. write to stdout [low-level]
(1!:1) 1                 NB. read from stdin

NB. ---------------------------------------------------------
NB. Error Handling
NB. ---------------------------------------------------------
assert. y                NB. y is an expression evaluating to a boolean [in explicit defs only]
(9!:34)''                NB. check if assertions are enabled
(9!:35) y                NB. set assertions enabled [y=0: disable, y=1: enable]
try. B1 catch. B2 end.   NB. try/catch control structure [in explicit defs only]
(f :: g) y               NB. evaluate f y. If and only if f y fails, evaluate g y
(13!:0) 1                NB. set suspend on error/exit suspend mode
(13!:0) 0                NB. disable suspend on error
(13!:17)''               NB. check if suspend on error is enabled
(13!:12)''               NB. error message
(13!:13)''               NB. stack trace [when in suspend mode]
(13!:6) y                NB. resume from suspend with result y
(13!:14)''               NB. get value of uncaught error handler
(13!:15) y               NB. set value of uncaught error handler [y is a J expression as string]

NB. ---------------------------------------------------------
NB. Performance
NB. ---------------------------------------------------------
6!:2 y                   NB. measure time in seconds to execute y [y is a J expression as string]
x 6!:2 y                 NB. " averaged over x repetitions
load 'jpm'               NB. load profiler
start_jpm_''             NB. start profiler
showdetail_jpm_ 'F'      NB. show profiling for function named F

NB. ---------------------------------------------------------
NB. Control
NB. ---------------------------------------------------------
NB. notes:
NB. - control structure as last executed item of an explicit definition will return the
NB.   structure's value

NB. conditional
NB. ===========
NB. T evalutes to true if its first raveled item is not unboxed 0
if. T do. B1 else. B2 end.
if. T1 do. B1 elseif. T2 do. B2 elseif. 1 do. B3 end.
if. T do. B end.

NB. select
NB. ======
select. n case. n1 do. B1 case. n2;n3 do. B2 case. do. B3 end.

NB. while/whilst
NB. ============

while. T do. B end.
whilst. T do. B end.     =   (B; while. T do. B end.)

NB. for
NB. ===
for. A do. B end.        NB. execute B #A times
for_elem. A do. B end.   NB. `elem` in scope of B for each element of A,
NB.                          `elem_index` to get the current index

NB. return
NB. ======
NB. The effect of the return. control word is to short-circuit any further execution of the verb,
NB. delivering the most-recently computed value
NB. e.g.
verb define
    if. T1 do. B1 return. end.
    if. T2 do. B2 return. end.
    B3
)

NB. ---------------------------------------------------------
NB. Evaluating Expressions
NB. ---------------------------------------------------------
NB. Rules
NB. =====
NB. 1 monad        EDGE VERB NOUN ...         => EDGE Z ...        Z = VERB NOUN
NB. 2 monad2       EAVN VERB1 VERB2 NOUN ...  => EAVN VERB1 Z ...  Z = VERB2 NOUN
NB. 3 dyad         EAVN NOUN1 VERB NOUN2 etc  => EAVN Z ...        Z = NOUN1 VERB NOUN2
NB. 4 adverb       EAVN VN ADVERB ...         => EAVN Z ...        Z = VN ADVERB
NB. 5 conjunction  EAVN VN1 CONJ VN1 ...      => EAVN Z ...        Z = VN1 CONJ VN2
NB. 6 trident      EAVN VN1 VERB2 VERB3 ...   => EAVN Z ...        Z = VN1 VERB2 VERB3
NB. 7 bident       EDGE CAVN1 CAVN2 ...       => EDGE Z ...        Z = CAVN1 CAVN2
NB. 8 assign       NN Asgn CAVN ...           => Z ...             Z = CAVN
NB. 9 paren        ( CAVN ) ...               => Z ...             Z = CAVN
NB.
NB. Glossary
NB. ========
NB. Mark --> Marks the beggining of the expression
NB. Asgn --> =: / =.
NB. EDGE --> Mark / Asgn / (
NB. NN   --> Name / Noun
NB. VN   --> Verb / Noun
NB. EAVN --> Edge / Adverb / Verb / Noun
NB. CAVN --> Conjunction / Adverb / Verb / Noun
NB.
NB. Bidents
NB. =======
NB. verb         verb          -->  verb (hook)
NB. adverb       adverb        -->  adverb
NB. conjunction  verb          -->  adverb
NB. conjunction  noun          -->  adverb
NB. verb         conjunction   -->  adverb
NB. noun         conjunction   -->  adverb
NB.
NB. Notes
NB. =====
NB. Names that have already been assigned a value when encountered are evaluated immediately;
NB. otherwise, it is assumed that they will become verbs
NB.
NB. Effects
NB. =======
NB. * verbs have long right scope
NB. * verbs have short left scope
NB. * adverbs are applied before verbs
NB. * conjunctions are applied before verbs
NB. * adverbs have long left scope
NB. * adverbs have short right scope
NB. * conjunctions have long left scope
NB. * conjunctions have short right scope
NB. * trains on the left can break long left scope of adverbs/conjunctions
