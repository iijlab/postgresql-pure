{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Database.PostgreSQL.Pure.Oid
  ( Oid (Oid)
  , _daterange
  , _int4range
  , _int8range
  , _numrange
  , _tsrange
  , _tstzrange
  , bit
  , bitArray
  , bool
  , boolArray
  , box
  , boxArray
  , bpchar
  , bpcharArray
  , bytea
  , byteaArray
  , char
  , charArray
  , cid
  , cidArray
  , cidr
  , cidrArray
  , circle
  , circleArray
  , date
  , dateArray
  , daterange
  , float4
  , float4Array
  , float8
  , float8Array
  , inet
  , inetArray
  , int2
  , int2Array
  , int2vector
  , int2vectorArray
  , int4
  , int4Array
  , int4range
  , int8
  , int8Array
  , int8range
  , interval
  , intervalArray
  , json
  , jsonArray
  , jsonb
  , jsonbArray
  , line
  , lineArray
  , lseg
  , lsegArray
  , macaddr
  , macaddrArray
  , money
  , moneyArray
  , name
  , nameArray
  , numeric
  , numericArray
  , numrange
  , oid
  , oidArray
  , oidvector
  , oidvectorArray
  , path
  , pathArray
  , point
  , pointArray
  , polygon
  , polygonArray
  , record
  , recordArray
  , refcursor
  , refcursorArray
  , regclass
  , regclassArray
  , regoper
  , regoperArray
  , regoperator
  , regoperatorArray
  , regproc
  , regprocArray
  , regprocedure
  , regprocedureArray
  , regtype
  , regtypeArray
  , sqlIdentifier
  , text
  , textArray
  , tid
  , tidArray
  , time
  , timeArray
  , timestamp
  , timestampArray
  , timestamptz
  , timestamptzArray
  , timetz
  , timetzArray
  , tsrange
  , tstzrange
  , unknown
  , uuid
  , uuidArray
  , varbit
  , varbitArray
  , varchar
  , varcharArray
  , void
  , xid
  , xidArray
  , xml
  , xmlArray
  ) where

import           Control.Applicative (Alternative ((<|>)))
import           Data.Int            (Int32)
import           Text.Read           (Read (readPrec))
import qualified Text.Read           as R
import qualified Text.Read.Lex       as R

-- | OID.
--
-- Constant values are listed in @Database.PostgreSQL.Pure.Oid@.
newtype Oid = Oid Int32 deriving (Eq, Num)

bool :: Oid
bool = 16

bytea :: Oid
bytea = 17

char :: Oid
char = 18

name :: Oid
name = 19

int8 :: Oid
int8 = 20

int2 :: Oid
int2 = 21

int4 :: Oid
int4 = 23

regproc :: Oid
regproc = 24

text :: Oid
text = 25

oid :: Oid
oid = 26

tid :: Oid
tid = 27

xid :: Oid
xid = 28

cid :: Oid
cid = 29

xml :: Oid
xml = 142

point :: Oid
point = 600

lseg :: Oid
lseg = 601

path :: Oid
path = 602

box :: Oid
box = 603

polygon :: Oid
polygon = 604

line :: Oid
line = 628

cidr :: Oid
cidr = 650

float4 :: Oid
float4 = 700

float8 :: Oid
float8 = 701

unknown :: Oid
unknown = 705

circle :: Oid
circle = 718

money :: Oid
money = 790

macaddr :: Oid
macaddr = 829

inet :: Oid
inet = 869

bpchar :: Oid
bpchar = 1042

varchar :: Oid
varchar = 1043

date :: Oid
date = 1082

time :: Oid
time = 1083

timestamp :: Oid
timestamp = 1114

timestamptz :: Oid
timestamptz = 1184

interval :: Oid
interval = 1186

timetz :: Oid
timetz = 1266

bit :: Oid
bit = 1560

varbit :: Oid
varbit = 1562

numeric :: Oid
numeric = 1700

refcursor :: Oid
refcursor = 1790

record :: Oid
record = 2249

void :: Oid
void = 2278

recordArray :: Oid
recordArray = 2287

regprocedure :: Oid
regprocedure = 2202

regoper :: Oid
regoper = 2203

regoperator :: Oid
regoperator = 2204

regclass :: Oid
regclass = 2205

regtype :: Oid
regtype = 2206

uuid :: Oid
uuid = 2950

json :: Oid
json = 114

jsonb :: Oid
jsonb = 3802

int2vector :: Oid
int2vector = 22

oidvector :: Oid
oidvector = 30

xmlArray :: Oid
xmlArray = 143

jsonArray :: Oid
jsonArray = 199

lineArray :: Oid
lineArray = 629

cidrArray :: Oid
cidrArray = 651

circleArray :: Oid
circleArray = 719

moneyArray :: Oid
moneyArray = 791

boolArray :: Oid
boolArray = 1000

byteaArray :: Oid
byteaArray = 1001

charArray :: Oid
charArray = 1002

nameArray :: Oid
nameArray = 1003

int2Array :: Oid
int2Array = 1005

int2vectorArray :: Oid
int2vectorArray = 1006

int4Array :: Oid
int4Array = 1007

regprocArray :: Oid
regprocArray = 1008

textArray :: Oid
textArray = 1009

tidArray :: Oid
tidArray = 1010

xidArray :: Oid
xidArray = 1011

cidArray :: Oid
cidArray = 1012

oidvectorArray :: Oid
oidvectorArray = 1013

bpcharArray :: Oid
bpcharArray = 1014

varcharArray :: Oid
varcharArray = 1015

int8Array :: Oid
int8Array = 1016

pointArray :: Oid
pointArray = 1017

lsegArray :: Oid
lsegArray = 1018

pathArray :: Oid
pathArray = 1019

boxArray :: Oid
boxArray = 1020

float4Array :: Oid
float4Array = 1021

float8Array :: Oid
float8Array = 1022

polygonArray :: Oid
polygonArray = 1027

oidArray :: Oid
oidArray = 1028

macaddrArray :: Oid
macaddrArray = 1040

inetArray :: Oid
inetArray = 1041

timestampArray :: Oid
timestampArray = 1115

dateArray :: Oid
dateArray = 1182

timeArray :: Oid
timeArray = 1183

timestamptzArray :: Oid
timestamptzArray = 1185

intervalArray :: Oid
intervalArray = 1187

numericArray :: Oid
numericArray = 1231

timetzArray :: Oid
timetzArray = 1270

bitArray :: Oid
bitArray = 1561

varbitArray :: Oid
varbitArray = 1563

refcursorArray :: Oid
refcursorArray = 2201

regprocedureArray :: Oid
regprocedureArray = 2207

regoperArray :: Oid
regoperArray = 2208

regoperatorArray :: Oid
regoperatorArray = 2209

regclassArray :: Oid
regclassArray = 2210

regtypeArray :: Oid
regtypeArray = 2211

uuidArray :: Oid
uuidArray = 2951

jsonbArray :: Oid
jsonbArray = 3807

int4range :: Oid
int4range = 3904

_int4range :: Oid
_int4range = 3905

numrange :: Oid
numrange = 3906

_numrange :: Oid
_numrange = 3907

tsrange :: Oid
tsrange = 3908

_tsrange :: Oid
_tsrange = 3909

tstzrange :: Oid
tstzrange = 3910

_tstzrange :: Oid
_tstzrange = 3911

daterange :: Oid
daterange = 3912

_daterange :: Oid
_daterange = 3913

int8range :: Oid
int8range = 3926

_int8range :: Oid
_int8range = 3927

sqlIdentifier :: Oid
sqlIdentifier = 12664

instance Show Oid where
  show o | o == bool = "bool"
         | o == bytea = "bytea"
         | o == char = "char"
         | o == name = "name"
         | o == int8 = "int8"
         | o == int2 = "int2"
         | o == int4 = "int4"
         | o == regproc = "regproc"
         | o == text = "text"
         | o == oid = "oid"
         | o == tid = "tid"
         | o == xid = "xid"
         | o == cid = "cid"
         | o == xml = "xml"
         | o == point = "point"
         | o == lseg = "lseg"
         | o == path = "path"
         | o == box = "box"
         | o == polygon = "polygon"
         | o == line = "line"
         | o == cidr = "cidr"
         | o == float4 = "float4"
         | o == float8 = "float8"
         | o == unknown = "unknown"
         | o == circle = "circle"
         | o == money = "money"
         | o == macaddr = "macaddr"
         | o == inet = "inet"
         | o == bpchar = "bpchar"
         | o == varchar = "varchar"
         | o == date = "date"
         | o == time = "time"
         | o == timestamp = "timestamp"
         | o == timestamptz = "timestamptz"
         | o == interval = "interval"
         | o == timetz = "timetz"
         | o == bit = "bit"
         | o == varbit = "varbit"
         | o == numeric = "numeric"
         | o == refcursor = "refcursor"
         | o == record = "record"
         | o == void = "void"
         | o == recordArray = "recordArray"
         | o == regprocedure = "regprocedure"
         | o == regoper = "regoper"
         | o == regoperator = "regoperator"
         | o == regclass = "regclass"
         | o == regtype = "regtype"
         | o == uuid = "uuid"
         | o == json = "json"
         | o == jsonb = "jsonb"
         | o == int2vector = "int2vector"
         | o == oidvector = "oidvector"
         | o == xmlArray = "xmlArray"
         | o == jsonArray = "jsonArray"
         | o == lineArray = "lineArray"
         | o == cidrArray = "cidrArray"
         | o == circleArray = "circleArray"
         | o == moneyArray = "moneyArray"
         | o == boolArray = "boolArray"
         | o == byteaArray = "byteaArray"
         | o == charArray = "charArray"
         | o == nameArray = "nameArray"
         | o == int2Array = "int2Array"
         | o == int2vectorArray = "int2vectorArray"
         | o == int4Array = "int4Array"
         | o == regprocArray = "regprocArray"
         | o == textArray = "textArray"
         | o == tidArray = "tidArray"
         | o == xidArray = "xidArray"
         | o == cidArray = "cidArray"
         | o == oidvectorArray = "oidvectorArray"
         | o == bpcharArray = "bpcharArray"
         | o == varcharArray = "varcharArray"
         | o == int8Array = "int8Array"
         | o == pointArray = "pointArray"
         | o == lsegArray = "lsegArray"
         | o == pathArray = "pathArray"
         | o == boxArray = "boxArray"
         | o == float4Array = "float4Array"
         | o == float8Array = "float8Array"
         | o == polygonArray = "polygonArray"
         | o == oidArray = "oidArray"
         | o == macaddrArray = "macaddrArray"
         | o == inetArray = "inetArray"
         | o == timestampArray = "timestampArray"
         | o == dateArray = "dateArray"
         | o == timeArray = "timeArray"
         | o == timestamptzArray = "timestamptzArray"
         | o == intervalArray = "intervalArray"
         | o == numericArray = "numericArray"
         | o == timetzArray = "timetzArray"
         | o == bitArray = "bitArray"
         | o == varbitArray = "varbitArray"
         | o == refcursorArray = "refcursorArray"
         | o == regprocedureArray = "regprocedureArray"
         | o == regoperArray = "regoperArray"
         | o == regoperatorArray = "regoperatorArray"
         | o == regclassArray = "regclassArray"
         | o == regtypeArray = "regtypeArray"
         | o == uuidArray = "uuidArray"
         | o == jsonbArray = "jsonbArray"
         | o == int4range = "int4range"
         | o == _int4range = "_int4range"
         | o == numrange = "numrange"
         | o == _numrange = "_numrange"
         | o == tsrange = "tsrange"
         | o == _tsrange = "_tsrange"
         | o == tstzrange = "tstzrange"
         | o == _tstzrange = "_tstzrange"
         | o == daterange = "daterange"
         | o == _daterange = "_daterange"
         | o == int8range = "int8range"
         | o == _int8range = "_int8range"
         | o == sqlIdentifier = "sqlIdentifier"
  show (Oid n) = show n

instance Read Oid where
  readPrec =
    R.parens
        ( do
            R.lift $ R.expect $ R.Ident "bool"
            pure bool
        )
        <|> ( do
                R.lift $ R.expect $ R.Ident "bytea"
                pure bytea
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "char"
                pure char
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "name"
                pure name
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "int8"
                pure int8
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "int2"
                pure int2
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "int4"
                pure int4
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regproc"
                pure regproc
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "text"
                pure text
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "oid"
                pure oid
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "tid"
                pure tid
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "xid"
                pure xid
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "cid"
                pure cid
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "xml"
                pure xml
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "point"
                pure point
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "lseg"
                pure lseg
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "path"
                pure path
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "box"
                pure box
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "polygon"
                pure polygon
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "line"
                pure line
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "cidr"
                pure cidr
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "float4"
                pure float4
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "float8"
                pure float8
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "unknown"
                pure unknown
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "circle"
                pure circle
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "money"
                pure money
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "macaddr"
                pure macaddr
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "inet"
                pure inet
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "bpchar"
                pure bpchar
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "varchar"
                pure varchar
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "date"
                pure date
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "time"
                pure time
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "timestamp"
                pure timestamp
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "timestamptz"
                pure timestamptz
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "interval"
                pure interval
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "timetz"
                pure timetz
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "bit"
                pure bit
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "varbit"
                pure varbit
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "numeric"
                pure numeric
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "refcursor"
                pure refcursor
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "record"
                pure record
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "void"
                pure void
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "recordArray"
                pure recordArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regprocedure"
                pure regprocedure
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regoper"
                pure regoper
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regoperator"
                pure regoperator
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regclass"
                pure regclass
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regtype"
                pure regtype
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "uuid"
                pure uuid
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "json"
                pure json
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "jsonb"
                pure jsonb
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "int2vector"
                pure int2vector
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "oidvector"
                pure oidvector
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "xmlArray"
                pure xmlArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "jsonArray"
                pure jsonArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "lineArray"
                pure lineArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "cidrArray"
                pure cidrArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "circleArray"
                pure circleArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "moneyArray"
                pure moneyArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "boolArray"
                pure boolArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "byteaArray"
                pure byteaArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "charArray"
                pure charArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "nameArray"
                pure nameArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "int2Array"
                pure int2Array
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "int2vectorArray"
                pure int2vectorArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "int4Array"
                pure int4Array
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regprocArray"
                pure regprocArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "textArray"
                pure textArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "tidArray"
                pure tidArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "xidArray"
                pure xidArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "cidArray"
                pure cidArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "oidvectorArray"
                pure oidvectorArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "bpcharArray"
                pure bpcharArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "varcharArray"
                pure varcharArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "int8Array"
                pure int8Array
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "pointArray"
                pure pointArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "lsegArray"
                pure lsegArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "pathArray"
                pure pathArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "boxArray"
                pure boxArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "float4Array"
                pure float4Array
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "float8Array"
                pure float8Array
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "polygonArray"
                pure polygonArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "oidArray"
                pure oidArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "macaddrArray"
                pure macaddrArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "inetArray"
                pure inetArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "timestampArray"
                pure timestampArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "dateArray"
                pure dateArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "timeArray"
                pure timeArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "timestamptzArray"
                pure timestamptzArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "intervalArray"
                pure intervalArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "numericArray"
                pure numericArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "timetzArray"
                pure timetzArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "bitArray"
                pure bitArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "varbitArray"
                pure varbitArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "refcursorArray"
                pure refcursorArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regprocedureArray"
                pure regprocedureArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regoperArray"
                pure regoperArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regoperatorArray"
                pure regoperatorArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regclassArray"
                pure regclassArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "regtypeArray"
                pure regtypeArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "uuidArray"
                pure uuidArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "jsonbArray"
                pure jsonbArray
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "int4range"
                pure int4range
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "_int4range"
                pure _int4range
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "numrange"
                pure numrange
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "_numrange"
                pure _numrange
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "tsrange"
                pure tsrange
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "_tsrange"
                pure _tsrange
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "tstzrange"
                pure tstzrange
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "_tstzrange"
                pure _tstzrange
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "daterange"
                pure daterange
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "_daterange"
                pure _daterange
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "int8range"
                pure int8range
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "_int8range"
                pure _int8range
            )
        <|> ( do
                R.lift $ R.expect $ R.Ident "sqlIdentifier"
                pure sqlIdentifier
            )
        <|> (Oid <$> readPrec)
