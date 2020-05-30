(*
proctree.js Copyright (c) 2012, Paul Brunt
c++ port Copyright (c) 2015, Jari Komppa
Pascal port Copyright (c) 2018, Jim Valavanis
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
* Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.
* Neither the name of proctree.js nor the
names of its contributors may be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL PAUL BRUNT BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

unit proctree;

interface

type
  fvec3_t = record
    x, y, z: single;
  end;
  fvec3_p = ^fvec3_t;
  fvec3_a = array[0..$FFFF] of fvec3_t;
  fvec3_pa = ^fvec3_a;

  fvec2_t = record
    u, v: single;
  end;
  fvec2_p = ^fvec2_t;
  fvec2_a = array[0..$FFFF] of fvec2_t;
  fvec2_pa = ^fvec2_a;

  ivec3_t = record
    x, y, z: integer;
  end;
  ivec3_p = ^ivec3_t;
  ivec3_a = array[0..$FFFF] of ivec3_t;
  ivec3_pa = ^ivec3_a;

type
  properties_t = class
  public
    mClumpMax: single;
    mClumpMin: single;
    mLengthFalloffFactor: single;
    mLengthFalloffPower: single;
    mBranchFactor: single;
    mRadiusFalloffRate: single;
    mClimbRate: single;
    mTrunkKink: single;
    mMaxRadius: single;
    mTreeSteps: integer;
    mTaperRate: single;
    mTwistRate: single;
    mSegments: integer;
    mLevels: integer;
    mSweepAmount: single;
    mInitialBranchLength: single;
    mTrunkLength: single;
    mDropAmount: single;
    mGrowAmount: single;
    mVMultiplier: single;
    mTwigScale: single;
    mSeed: integer;
    mRseed: integer;
    constructor CreateDefault; virtual;
    constructor Create(
      aClumpMax: single;
      aClumpMin: single;
      aLengthFalloffFactor: single;
      aLengthFalloffPower: single;
      aBranchFactor: single;
      aRadiusFalloffRate: single;
      aClimbRate: single;
      aTrunkKink: single;
      aMaxRadius: single;
      aTreeSteps: integer;
      aTaperRate: single;
      aTwistRate: single;
      aSegments: integer;
      aLevels: integer;
      aSweepAmount: single;
      aInitialBranchLength: single;
      aTrunkLength: single;
      aDropAmount: single;
      aGrowAmount: single;
      aVMultiplier: single;
      aTwigScale: single;
      aSeed: integer;
      aRseed: integer
    ); virtual;
    procedure DefaultValues(const seed: integer);
    function random(aFixed: single): single;
  end;


  branch_t = class
  public
    mChild0: branch_t;
    mChild1: branch_t;
    mParent: branch_t;
    mHead: fvec3_t;
    mTangent: fvec3_t;
    mLength: single;
    mTrunktype: integer;
    mRing0, mRing1, mRing2: array of integer;
    mRootRing: array of integer;
    mRadius: single;
    mEnd: integer;
    constructor CreateDefault; virtual;
    constructor Create(const aHead: fvec3_t; const aParent: branch_t); virtual;
    destructor Destroy; override;
    procedure split(aLevel: integer; aSteps: integer; aProperties: properties_t; aL1: integer = 1; aL2: integer = 1);
  end;

  tree_t = class
  protected
    mRoot: branch_t;
    procedure init;
    procedure allocVertBuffers;
    procedure allocFaceBuffers;
    procedure calcVertSizes(aBranch: branch_t);
    procedure calcFaceSizes(aBranch: branch_t);
    procedure calcNormals;
    procedure doFaces(aBranch: branch_t);
    procedure createTwigs(aBranch: branch_t);
    procedure createForks(aBranch: branch_t; aRadius: single);
    procedure fixUVs;
  public
    mProperties: properties_t;
    mVertCount: integer;
    mTwigVertCount: integer;
    mFaceCount: integer;
    mTwigFaceCount: integer;

    mVert: array of fvec3_t;
    mNormal: array of fvec3_t;
    mUV: array of fvec2_t;
    mTwigVert: array of fvec3_t;
    mTwigNormal: array of fvec3_t;
    mTwigUV: array of fvec2_t;
    mFace: array of ivec3_t;
    mTwigFace: array of ivec3_t;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure generate;
  end;

implementation

uses
  Math;

const
  M_PI = 3.1415926535897932384626433832795;

function makefvec3(const x, y, z: single): fvec3_t;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

function makefvec2(const u, v: single): fvec2_t;
begin
  result.u := u;
  result.v := v;
end;

function makeivec3(const x, y, z: integer): ivec3_t;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

function fv3length(const a: fvec3_t): single;
begin
  result := sqrt(a.x * a.x + a.y * a.y + a.z * a.z);
end;

function fv3normalize(const a: fvec3_t): fvec3_t;
var
  l: single;
begin
  l := fv3length(a);
  result := a;
  if l <> 0.0 then
  begin
    l := 1.0 / l;
    result.x := result.x * l;
    result.y := result.y * l;
    result.z := result.z * l;
  end;
end;

function fv3cross(const a, b: fvec3_t): fvec3_t;
begin
  result.x := a.y * b.z - a.z * b.y;
  result.y := a.z * b.x - a.x * b.z;
  result.z := a.x * b.y - a.y * b.x;
end;

function fv3dot(const a, b: fvec3_t): single;
begin
  result := a.x * b.x + a.y * b.y + a.z * b.z;
end;

function fv3sub(const a, b: fvec3_t): fvec3_t;
begin
  result.x := a.x - b.x;
  result.y := a.y - b.y;
  result.z := a.z - b.z;
end;

function fv3add(const a, b: fvec3_t): fvec3_t;
begin
  result.x := a.x + b.x;
  result.y := a.y + b.y;
  result.z := a.z + b.z;
end;

function scaleVec(const a: fvec3_t; const b: single): fvec3_t;
begin
  result.x := a.x * b;
  result.y := a.y * b;
  result.z := a.z * b;
end;

function scaleInDirection(const aVector, aDirection: fvec3_t; const aScale: single): fvec3_t;
var
  currentMag: single;
  change: fvec3_t;
begin
  currentMag := fv3dot(aVector, aDirection);

  change := scaleVec(aDirection, currentMag * aScale - currentMag);
  result := fv3add(aVector, change);
end;

function vecAxisAngle(const aVec, aAxis: fvec3_t; const aAngle: single): fvec3_t;
var
  cosr, sinr: single;
begin
  cosr := cos(aAngle);
  sinr := sin(aAngle);
  result := fv3add(fv3add(scaleVec(aVec, cosr), scaleVec(fv3cross(aAxis, aVec), sinr)),
                 scaleVec(aAxis, fv3dot(aAxis, aVec) * (1 - cosr)));
end;

function mirrorBranch(const aVec, aNorm: fvec3_t; const aProperties: properties_t): fvec3_t;
var
  v: fvec3_t;
  s: single;
begin
  v := fv3cross(aNorm, fv3cross(aVec, aNorm));
  s := aProperties.mBranchFactor * fv3dot(v, aVec);
  result.x := aVec.x - v.x * s;
  result.y := aVec.y - v.y * s;
  result.z := aVec.z - v.z * s;
end;

// properties_t
constructor properties_t.CreateDefault;
begin
  DefaultValues(262);
end;

constructor properties_t.Create(
  aClumpMax: single;
  aClumpMin: single;
  aLengthFalloffFactor: single;
  aLengthFalloffPower: single;
  aBranchFactor: single;
  aRadiusFalloffRate: single;
  aClimbRate: single;
  aTrunkKink: single;
  aMaxRadius: single;
  aTreeSteps: integer;
  aTaperRate: single;
  aTwistRate: single;
  aSegments: integer;
  aLevels: integer;
  aSweepAmount: single;
  aInitialBranchLength: single;
  aTrunkLength: single;
  aDropAmount: single;
  aGrowAmount: single;
  aVMultiplier: single;
  aTwigScale: single;
  aSeed: integer;
  aRseed: integer
);
begin
  mSeed := aSeed;
  mSegments := aSegments;
  mLevels := aLevels;
  mVMultiplier := aVMultiplier;
  mTwigScale := aTwigScale;
  mInitialBranchLength := aInitialBranchLength;
  mLengthFalloffFactor := aLengthFalloffFactor;
  mLengthFalloffPower := aLengthFalloffPower;
  mClumpMax := aClumpMax;
  mClumpMin := aClumpMin;
  mBranchFactor := aBranchFactor;
  mDropAmount := aDropAmount;
  mGrowAmount := aGrowAmount;
  mSweepAmount := aSweepAmount;
  mMaxRadius := aMaxRadius;
  mClimbRate := aClimbRate;
  mTrunkKink := aTrunkKink;
  mTreeSteps := aTreeSteps;
  mTaperRate := aTaperRate;
  mRadiusFalloffRate := aRadiusFalloffRate;
  mTwistRate := aTwistRate;
  mTrunkLength := aTrunkLength;
end;

procedure properties_t.DefaultValues(const seed: integer);
begin
  mSeed := seed;
  mSegments := 6;
  mLevels := 5;
  mVMultiplier := 0.36;
  mTwigScale := 0.39;
  mInitialBranchLength := 0.49;
  mLengthFalloffFactor := 0.85;
  mLengthFalloffPower := 0.99;
  mClumpMax := 0.454;
  mClumpMin := 0.404;
  mBranchFactor := 2.45;
  mDropAmount := -0.1;
  mGrowAmount := 0.235;
  mSweepAmount := 0.01;
  mMaxRadius := 0.139;
  mClimbRate := 0.371;
  mTrunkKink := 0.093;
  mTreeSteps := 5;
  mTaperRate := 0.947;
  mRadiusFalloffRate := 0.73;
  mTwistRate := 3.02;
  mTrunkLength := 2.4;
end;

function properties_t.random(aFixed: single): single;
begin
  if aFixed = 0 then
  begin
    aFixed := mRseed;
    inc(mRseed);
  end;
  result := abs(cos(aFixed + aFixed * aFixed));
end;

// branch_t
constructor branch_t.CreateDefault;
begin
  SetLength(mRootRing, 0);
  SetLength(mRing0, 0);
  SetLength(mRing1, 0);
  SetLength(mRing2, 0);
  mChild0 := nil;
  mChild1 := nil;
  mParent := nil;
  mLength := 1.0;
  mTrunktype := 0;
  mRadius := 0.0;
  mHead := makefvec3(0.0, 0.0, 0.0);
  mTangent := makefvec3(0.0, 0.0, 0.0);
  mEnd := 0;
end;

constructor branch_t.Create(const aHead: fvec3_t; const aParent: branch_t);
begin
  SetLength(mRootRing, 0);
  SetLength(mRing0, 0);
  SetLength(mRing1, 0);
  SetLength(mRing2, 0);
  mChild0 := nil;
  mChild1 := nil;
  mParent := aParent;
  mLength := 1.0;
  mTrunktype := 0;
  mRadius := 0.0;
  mHead := aHead;
  mTangent := makefvec3(0.0, 0.0, 0.0);
  mEnd := 0;
end;

destructor branch_t.Destroy;
begin
  mChild0.Free;
  mChild1.Free;
  SetLength(mRootRing, 0);
  SetLength(mRing0, 0);
  SetLength(mRing1, 0);
  SetLength(mRing2, 0);
end;

procedure branch_t.split(aLevel: integer; aSteps: integer; aProperties: properties_t; aL1: integer = 1; aL2: integer = 1);
var
  rLevel: integer;
  po: fvec3_t;
  so: fvec3_t;
  dir: fvec3_t;
  a: fvec3_t;
  normal: fvec3_t;
  tangent: fvec3_t;
  r: single;
  adj: fvec3_t;
  clump: single;
  newdir: fvec3_t;
  newdir2: fvec3_t;
  tmp: fvec3_t;
  angle: single;
  growAmount: single;
  dropAmount: single;
  sweepAmount: single;
  head0: fvec3_t;
  head1: fvec3_t;
begin
  rLevel := aProperties.mLevels - aLevel;
  if mParent <> nil then
    po := mParent.mHead
  else
  begin
    po := makefvec3(0.0, 0.0, 0.0);
    mTrunktype := 1;
  end;

  so := mHead;
  dir := fv3normalize(fv3sub(so, po));

  a := makefvec3(dir.z, dir.x, dir.y);
  normal := fv3cross(dir, a);
  tangent := fv3cross(dir, normal);
  r := aProperties.random(rLevel * 10 + aL1 * 5.0 + aL2 + aProperties.mSeed);

  adj := fv3add(scaleVec(normal, r), scaleVec(tangent, 1 - r));
  if r > 0.5 then adj := scaleVec(adj, -1);

  clump := (aProperties.mClumpMax - aProperties.mClumpMin) * r + aProperties.mClumpMin;
  newdir := fv3normalize(fv3add(scaleVec(adj, 1 - clump), scaleVec(dir, clump)));

  newdir2 := mirrorBranch(newdir, dir, aProperties);
  if r > 0.5 then
  begin
    tmp := newdir;
    newdir := newdir2;
    newdir2 := tmp;
  end;

  if aSteps > 0 then
  begin
    angle := aSteps / aProperties.mTreeSteps * 2 * M_PI * aProperties.mTwistRate;
    a := makefvec3(sin(angle), r, cos(angle));
    newdir2 := fv3normalize(a);
  end;

  growAmount := aLevel * aLevel / (aProperties.mLevels * aProperties.mLevels) * aProperties.mGrowAmount;
  dropAmount := rLevel * aProperties.mDropAmount;
  sweepAmount := rLevel * aProperties.mSweepAmount;
  a := makefvec3(sweepAmount, dropAmount + growAmount, 0.0);
  newdir := fv3normalize(fv3add(newdir, a));
  newdir2 := fv3normalize(fv3add(newdir2, a));

  head0 := fv3add(so, scaleVec(newdir, mLength));
  head1 := fv3add(so, scaleVec(newdir2, mLength));
  mChild0 := branch_t.Create(head0, self);
  mChild1 := branch_t.Create(head1, self);
  mChild0.mLength := power(mLength, aProperties.mLengthFalloffPower) * aProperties.mLengthFalloffFactor;
  mChild1.mLength := power(mLength, aProperties.mLengthFalloffPower) * aProperties.mLengthFalloffFactor;

  if aLevel > 0 then
  begin
    if aSteps > 0 then
    begin
      a.x := (r - 0.5) * 2 * aProperties.mTrunkKink;
      a.y := aProperties.mClimbRate;
      a.z := (r - 0.5) * 2 * aProperties.mTrunkKink;

      mChild0.mHead := fv3add(mHead, a);
      mChild0.mTrunktype := 1;
      mChild0.mLength := mLength * aProperties.mTaperRate;
      mChild0.split(aLevel, aSteps - 1, aProperties, aL1 + 1, aL2);
    end
    else
    begin
      mChild0.split(aLevel - 1, 0, aProperties, aL1 + 1, aL2);
    end;
    mChild1.split(aLevel - 1, 0, aProperties, aL1, aL2 + 1);
  end;
end;


constructor tree_t.Create;
begin
  mRoot := nil;
  mProperties := properties_t.CreateDefault;

  SetLength(mVert, 0);
  SetLength(mNormal, 0);
  SetLength(mUV, 0);
  SetLength(mTwigVert, 0);
  SetLength(mTwigNormal, 0);
  SetLength(mTwigUV, 0);
  SetLength(mFace, 0);
  SetLength(mTwigFace, 0);

  mVertCount := 0;
  mTwigVertCount := 0;
  mFaceCount := 0;
  mTwigFaceCount := 0;
end;

destructor tree_t.Destroy;
begin
  mRoot.Free;
  mProperties.Free;
  SetLength(mVert, 0);
  SetLength(mNormal, 0);
  SetLength(mUV, 0);
  SetLength(mTwigVert, 0);
  SetLength(mTwigNormal, 0);
  SetLength(mTwigUV, 0);
  SetLength(mFace, 0);
  SetLength(mTwigFace, 0);
end;

procedure tree_t.generate;
var
  starthead: fvec3_t;
begin
  init;
  mProperties.mRseed := mProperties.mSeed;
  starthead := makefvec3(0.0, mProperties.mTrunkLength, 0.0);
  mRoot := branch_t.Create(starthead, nil);
  mRoot.mLength := mProperties.mInitialBranchLength;
  mRoot.split(mProperties.mLevels, mProperties.mTreeSteps, mProperties);

  calcVertSizes(nil);
  allocVertBuffers;
  createForks(nil, 0.0);
  createTwigs(nil);
  calcFaceSizes(nil);
  allocFaceBuffers;
  doFaces(nil);
  calcNormals;
  fixUVs;
  mRoot.Free;
  mRoot := nil;
end;

procedure tree_t.init;
begin
  mRoot.Free;
  mRoot := nil;
  SetLength(mVert, 0);
  SetLength(mNormal, 0);
  SetLength(mUV, 0);
  SetLength(mTwigVert, 0);
  SetLength(mTwigNormal, 0);
  SetLength(mTwigUV, 0);
  SetLength(mFace, 0);
  SetLength(mTwigFace, 0);

  mVertCount := 0;
  mTwigVertCount := 0;
  mFaceCount := 0;
  mTwigFaceCount := 0;
end;

procedure tree_t.allocVertBuffers;
begin
  SetLength(mVert, mVertCount);
  SetLength(mNormal, mVertCount);
  SetLength(mUV, mVertCount);
  SetLength(mTwigVert, mTwigVertCount);
  SetLength(mTwigNormal, mTwigVertCount);
  SetLength(mTwigUV, mTwigVertCount);
  SetLength(mTwigFace, mTwigFaceCount);

  // Reset back to zero, we'll use these as counters
  mVertCount := 0;
  mTwigVertCount := 0;
  mTwigFaceCount := 0;
end;

procedure tree_t.allocFaceBuffers;
begin
  SetLength(mFace, mFaceCount);

  // Reset back to zero, we'll use these as counters
  mFaceCount := 0;
end;

procedure tree_t.calcVertSizes(aBranch: branch_t);
var
  segments: integer;
begin
  segments := mProperties.mSegments;
  if aBranch = nil then
    aBranch := mRoot;

  if aBranch.mParent = nil then
    inc(mVertCount, segments);

  if aBranch.mChild0 <> nil then
  begin
    mVertCount := mVertCount +
        1 +
        (segments div 2) - 1 +
        1 +
        (segments div 2) - 1 +
        (segments div 2) - 1;

    calcVertSizes(aBranch.mChild0);
    calcVertSizes(aBranch.mChild1);
  end
  else
  begin
    inc(mVertCount);
    inc(mTwigVertCount, 8);
    inc(mTwigFaceCount, 4);
  end;
end;

procedure tree_t.calcFaceSizes(aBranch: branch_t);
var
  segments: integer;
begin
  segments := mProperties.mSegments;
  if aBranch = nil then
    aBranch := mRoot;

  if aBranch.mParent = nil then
    inc(mFaceCount, segments * 2);

  if Length(aBranch.mChild0.mRing0) <> 0 then
  begin
    inc(mFaceCount, segments * 4);

    calcFaceSizes(aBranch.mChild0);
    calcFaceSizes(aBranch.mChild1);
  end
  else
    inc(mFaceCount, segments * 2);
end;

procedure tree_t.calcNormals;
var
  normalCount: array of integer;
  i: integer;
  norm: fvec3_t;
  d: single;
begin
  SetLength(normalCount, mVertCount);
  for i := 0 to mVertCount - 1 do
    normalCount[i] := 0;

  for i := 0 to mFaceCount - 1 do
  begin
    inc(normalCount[mFace[i].x]);
    inc(normalCount[mFace[i].y]);
    inc(normalCount[mFace[i].z]);

    norm := fv3normalize(fv3cross(fv3sub(mVert[mFace[i].y], mVert[mFace[i].z]), fv3sub(mVert[mFace[i].y], mVert[mFace[i].x])));

    mNormal[mFace[i].x] := fv3add(mNormal[mFace[i].x], norm);
    mNormal[mFace[i].y] := fv3add(mNormal[mFace[i].y], norm);
    mNormal[mFace[i].z] := fv3add(mNormal[mFace[i].z], norm);
  end;

  for i := 0 to mVertCount - 1 do
  begin
    d := 1.0 / normalCount[i];
    mNormal[i] := scaleVec(mNormal[i], d);
  end;

  SetLength(normalCount, 0);
end;

function inc_(var x: integer): integer;
begin
  result := x;
  inc(x);
end;

procedure tree_t.doFaces(aBranch: branch_t);
var
  segments: integer;
  i: integer;
  tangent: fvec3_t;
  normal: fvec3_t;
  left: fvec3_t;
  angle: single;
  segOffset: integer;
  iv1, iv2, iv3, iv4: integer;
  a: ivec3_t;
  len: single;
  segOffset0, segOffset1: integer;
  v1, v2: fvec3_t;
  match0, match1: single;
  d: fvec3_t;
  l: single;
  UVScale: single;
  len1, len2: single;
  uv1, uv2: fvec2_t;
begin
  if aBranch = nil then
    aBranch := mRoot;

  segments := mProperties.mSegments;

  if aBranch.mParent = nil then
  begin
    tangent := fv3normalize(fv3cross(fv3sub(aBranch.mChild0.mHead, aBranch.mHead), fv3sub(aBranch.mChild1.mHead, aBranch.mHead)));
    normal := fv3normalize(aBranch.mHead);
    left := makefvec3(-1.0, 0.0, 0.0);
    angle := ArcCos(fv3dot(tangent, left));
    if fv3dot(fv3cross(left, tangent), normal) > 0.0 then
      angle := 2 * M_PI - angle;

    segOffset := floor(0.5 + (angle / M_PI / 2 * segments));
    for i := 0 to segments - 1 do
    begin
      iv1 := aBranch.mRing0[i];
      iv2 := aBranch.mRootRing[(i + segOffset + 1) mod segments];
      iv3 := aBranch.mRootRing[(i + segOffset) mod segments];
      iv4 := aBranch.mRing0[(i + 1) mod segments];

      a := makeivec3(iv1, iv4, iv3);
      mFace[inc_(mFaceCount)] := a;
      a := makeivec3(iv4, iv2, iv3);
      mFace[inc_(mFaceCount)] := a;

      mUV[(i + segOffset) mod segments] := makefvec2(i / segments, 0.0);

      len := fv3length(fv3sub(mVert[aBranch.mRing0[i]], mVert[aBranch.mRootRing[(i + segOffset) mod segments]])) * mProperties.mVMultiplier;
      mUV[aBranch.mRing0[i]] := makefvec2(i / segments, len);
      mUV[aBranch.mRing2[i]] := makefvec2(i / segments, len);
    end;
  end;

  if Length(aBranch.mChild0.mRing0) <> 0 then
  begin
    segOffset0 := -1;
    segOffset1 := -1;
    match0 := 0.0;
    match1 := 0.0;

    v1 := fv3normalize(fv3sub(mVert[aBranch.mRing1[0]], aBranch.mHead));
    v2 := fv3normalize(fv3sub(mVert[aBranch.mRing2[0]], aBranch.mHead));

    v1 := scaleInDirection(v1, fv3normalize(fv3sub(aBranch.mChild0.mHead, aBranch.mHead)), 0.0);
    v2 := scaleInDirection(v2, fv3normalize(fv3sub(aBranch.mChild1.mHead, aBranch.mHead)), 0.0);

    for i := 0 to segments - 1 do
    begin
      d := fv3normalize(fv3sub(mVert[aBranch.mChild0.mRing0[i]], aBranch.mChild0.mHead));
      l := fv3dot(d, v1);
      if (segOffset0 = -1) or (l > match0) then
      begin
        match0 := l;
        segOffset0 := segments - i;
      end;
      d := fv3normalize(fv3sub(mVert[aBranch.mChild1.mRing0[i]], aBranch.mChild1.mHead));
      l := fv3dot(d, v2);
      if (segOffset1 = -1) or (l > match1) then
      begin
        match1 := l;
        segOffset1 := segments - i;
      end;
    end;

    UVScale := mProperties.mMaxRadius / aBranch.mRadius;

    for i := 0 to segments - 1 do
    begin
      iv1 := aBranch.mChild0.mRing0[i];
      iv2 := aBranch.mRing1[(i + segOffset0 + 1) mod segments];
      iv3 := aBranch.mRing1[(i + segOffset0) mod segments];
      iv4 := aBranch.mChild0.mRing0[(i + 1) mod segments];

      a := makeivec3(iv1, iv4, iv3);
      mFace[inc_(mFaceCount)] := a;
      a := makeivec3(iv4, iv2, iv3);
      mFace[inc_(mFaceCount)] := a;

      iv1 := aBranch.mChild1.mRing0[i];
      iv2 := aBranch.mRing2[(i + segOffset1 + 1) mod segments];
      iv3 := aBranch.mRing2[(i + segOffset1) mod segments];
      iv4 := aBranch.mChild1.mRing0[(i + 1) mod segments];

      a := makeivec3(iv1, iv2, iv3);
      mFace[inc_(mFaceCount)] := a;
      a := makeivec3(iv1, iv4, iv2);
      mFace[inc_(mFaceCount)] := a;

      len1 := fv3length(fv3sub(mVert[aBranch.mChild0.mRing0[i]], mVert[aBranch.mRing1[(i + segOffset0) mod segments]])) * UVScale;
      uv1 := mUV[aBranch.mRing1[(i + segOffset0 - 1) mod segments]];

      mUV[aBranch.mChild0.mRing0[i]] := makefvec2(uv1.u, uv1.v + len1 * mProperties.mVMultiplier);
      mUV[aBranch.mChild0.mRing2[i]] := makefvec2(uv1.u, uv1.v + len1 * mProperties.mVMultiplier);

      len2 := fv3length(fv3sub(mVert[aBranch.mChild1.mRing0[i]], mVert[aBranch.mRing2[(i + segOffset1) mod segments]])) * UVScale;
      uv2 := mUV[aBranch.mRing2[(i + segOffset1 - 1) mod segments]];

      mUV[aBranch.mChild1.mRing0[i]] := makefvec2(uv2.u, uv2.v + len2 * mProperties.mVMultiplier);
      mUV[aBranch.mChild1.mRing2[i]] := makefvec2(uv2.u, uv2.v + len2 * mProperties.mVMultiplier);
    end;

    doFaces(aBranch.mChild0);
    doFaces(aBranch.mChild1);
  end
  else
  begin
    for i := 0 to segments - 1 do
    begin
      a := makeivec3(
              aBranch.mChild0.mEnd,
              aBranch.mRing1[(i + 1) mod segments],
              aBranch.mRing1[i]
            );
      mFace[inc_(mFaceCount)] := a;

      a := makeivec3(
              aBranch.mChild1.mEnd,
              aBranch.mRing2[(i + 1) mod segments],
              aBranch.mRing2[i]
            );
      mFace[inc_(mFaceCount)] := a;

      len := fv3length(fv3sub(mVert[aBranch.mChild0.mEnd], mVert[aBranch.mRing1[i]]));
      mUV[aBranch.mChild0.mEnd] := makefvec2(i / segments - 1, len * mProperties.mVMultiplier);
      len := fv3length(fv3sub(mVert[aBranch.mChild1.mEnd], mVert[aBranch.mRing2[i]]));
      mUV[aBranch.mChild1.mEnd] := makefvec2(i / segments, len * mProperties.mVMultiplier);
    end;
  end;
end;

procedure tree_t.createTwigs(aBranch: branch_t);
var
  tangent: fvec3_t;
  binormal: fvec3_t;
  vert1, vert2, vert3, vert4, vert5, vert6, vert7, vert8: integer;
  normal1, normal2: fvec3_t;
begin
  if aBranch = nil then
    aBranch := mRoot;

  if aBranch.mChild0 = nil then
  begin
    tangent := fv3normalize(fv3cross(fv3sub(aBranch.mParent.mChild0.mHead, aBranch.mParent.mHead), fv3sub(aBranch.mParent.mChild1.mHead, aBranch.mParent.mHead)));
    binormal := fv3normalize(fv3sub(aBranch.mHead, aBranch.mParent.mHead));

    vert1 := mTwigVertCount;
    mTwigVert[inc_(mTwigVertCount)] := (fv3add(fv3add(aBranch.mHead, scaleVec(tangent, mProperties.mTwigScale)), scaleVec(binormal, mProperties.mTwigScale * 2 - aBranch.mLength)));
    vert2 := mTwigVertCount;
    mTwigVert[inc_(mTwigVertCount)] := (fv3add(fv3add(aBranch.mHead, scaleVec(tangent, -mProperties.mTwigScale)), scaleVec(binormal, mProperties.mTwigScale * 2 - aBranch.mLength)));
    vert3 := mTwigVertCount;
    mTwigVert[inc_(mTwigVertCount)] := (fv3add(fv3add(aBranch.mHead, scaleVec(tangent, -mProperties.mTwigScale)), scaleVec(binormal, -aBranch.mLength)));
    vert4 := mTwigVertCount;
    mTwigVert[inc_(mTwigVertCount)] := (fv3add(fv3add(aBranch.mHead, scaleVec(tangent, mProperties.mTwigScale)), scaleVec(binormal, -aBranch.mLength)));

    vert8 := mTwigVertCount;
    mTwigVert[inc_(mTwigVertCount)] := (fv3add(fv3add(aBranch.mHead, scaleVec(tangent, mProperties.mTwigScale)), scaleVec(binormal, mProperties.mTwigScale * 2 - aBranch.mLength)));
    vert7 := mTwigVertCount;
    mTwigVert[inc_(mTwigVertCount)] := (fv3add(fv3add(aBranch.mHead, scaleVec(tangent, -mProperties.mTwigScale)), scaleVec(binormal, mProperties.mTwigScale * 2 - aBranch.mLength)));
    vert6 := mTwigVertCount;
    mTwigVert[inc_(mTwigVertCount)] := (fv3add(fv3add(aBranch.mHead, scaleVec(tangent, -mProperties.mTwigScale)), scaleVec(binormal, -aBranch.mLength)));
    vert5 := mTwigVertCount;
    mTwigVert[inc_(mTwigVertCount)] := (fv3add(fv3add(aBranch.mHead, scaleVec(tangent, mProperties.mTwigScale)), scaleVec(binormal, -aBranch.mLength)));

    mTwigFace[inc_(mTwigFaceCount)] := makeivec3(vert1, vert2, vert3);
    mTwigFace[inc_(mTwigFaceCount)] := makeivec3(vert4, vert1, vert3);
    mTwigFace[inc_(mTwigFaceCount)] := makeivec3(vert6, vert7, vert8);
    mTwigFace[inc_(mTwigFaceCount)] := makeivec3(vert6, vert8, vert5);

    normal1 := fv3normalize(fv3cross(fv3sub(mTwigVert[vert1], mTwigVert[vert3]), fv3sub(mTwigVert[vert2], mTwigVert[vert3])));
    normal2 := fv3normalize(fv3cross(fv3sub(mTwigVert[vert7], mTwigVert[vert6]), fv3sub(mTwigVert[vert8], mTwigVert[vert6])));

    mTwigNormal[vert1] := normal1;
    mTwigNormal[vert2] := normal1;
    mTwigNormal[vert3] := normal1;
    mTwigNormal[vert4] := normal1;

    mTwigNormal[vert8] := normal2;
    mTwigNormal[vert7] := normal2;
    mTwigNormal[vert6] := normal2;
    mTwigNormal[vert5] := normal2;

    mTwigUV[vert1] := makefvec2(0.0, 0.0);
    mTwigUV[vert2] := makefvec2(1.0, 0.0);
    mTwigUV[vert3] := makefvec2(1.0, 1.0);
    mTwigUV[vert4] := makefvec2(0.0, 1.0);

    mTwigUV[vert8] := makefvec2(0.0, 0.0);
    mTwigUV[vert7] := makefvec2(1.0, 0.0);
    mTwigUV[vert6] := makefvec2(1.0, 1.0);
    mTwigUV[vert5] := makefvec2(0.0, 1.0);
  end
  else
  begin
    createTwigs(aBranch.mChild0);
    createTwigs(aBranch.mChild1);
  end;
end;

procedure tree_t.createForks(aBranch: branch_t; aRadius: single);
var
  segments: integer;
  segmentAngle: single;
  axis: fvec3_t;
  i: integer;
  left: fvec3_t;
  vec: fvec3_t;
  axis1, axis2, axis3: fvec3_t;
  tangent: fvec3_t;
  dir: fvec3_t;
  centerloc: fvec3_t;
  ring0count, ring1count, ring2count: integer;
  scale: single;
  linch0, linch1: integer;
  start: integer;
  d1, d2: fvec3_t;
  s: single;
  radius0, radius1: single;
begin
  if aBranch = nil then
    aBranch := mRoot;

  if aRadius = 0.0 then
    aRadius := mProperties.mMaxRadius;

  aBranch.mRadius := aRadius;

  if aRadius > aBranch.mLength then
    aRadius := aBranch.mLength;

  segments := mProperties.mSegments;

  segmentAngle := M_PI * 2 / segments;

  if aBranch.mParent = nil then
  begin
    SetLength(aBranch.mRootRing, segments);
    //create the root of the tree
    //branch.root = [];
    axis := makefvec3(0.0, 1.0, 0.0);

    for i := 0 to segments - 1 do
    begin
      left := makefvec3(-1.0, 0.0, 0.0);
      vec := vecAxisAngle(left, axis, -segmentAngle * i);
      aBranch.mRootRing[i] := mVertCount;
      mVert[inc_(mVertCount)] := (scaleVec(vec, aRadius / mProperties.mRadiusFalloffRate));
    end;
  end;

  //cross the branches to get the left
  //add the branches to get the up
  if aBranch.mChild0 <> nil then
  begin
    if aBranch.mParent <> nil then
      axis := fv3normalize(fv3sub(aBranch.mHead, aBranch.mParent.mHead))
    else
      axis := fv3normalize(aBranch.mHead);

    axis1 := fv3normalize(fv3sub(aBranch.mHead, aBranch.mChild0.mHead));
    axis2 := fv3normalize(fv3sub(aBranch.mHead, aBranch.mChild1.mHead));
    tangent := fv3normalize(fv3cross(axis1, axis2));
    aBranch.mTangent := tangent;

    axis3 := fv3normalize(fv3cross(tangent, fv3normalize(fv3add(scaleVec(axis1, -1.0), scaleVec(axis2, -1.0)))));
    dir := makefvec3(axis2.x, 0.0, axis2.z);
    centerloc := fv3add(aBranch.mHead, scaleVec(dir, -mProperties.mMaxRadius / 2));

    SetLength(aBranch.mRing0, segments);
    SetLength(aBranch.mRing1, segments);
    SetLength(aBranch.mRing2, segments);

    ring0count := 0;
    ring1count := 0;
    ring2count := 0;

    scale := mProperties.mRadiusFalloffRate;

    if (aBranch.mChild0.mTrunktype <> 0) or (aBranch.mTrunktype <> 0) then
      scale := 1.0 / mProperties.mTaperRate;

    //main segment ring
    linch0 := mVertCount;
    aBranch.mRing0[inc_(ring0count)] := linch0;
    aBranch.mRing2[inc_(ring2count)] := linch0;
    mVert[inc_(mVertCount)] := fv3add(centerloc, scaleVec(tangent, aRadius * scale));

    start := mVertCount - 1;
    d1 := vecAxisAngle(tangent, axis2, 1.57);
    d2 := fv3normalize(fv3cross(tangent, axis));
    s := 1 / fv3dot(d1, d2);

    for i := 1 to segments div 2 - 1 do
    begin
      vec := vecAxisAngle(tangent, axis2, segmentAngle * i);
      aBranch.mRing0[inc_(ring0count)] := start + i;
      aBranch.mRing2[inc_(ring2count)] := start + i;
      vec := scaleInDirection(vec, d2, s);
      mVert[inc_(mVertCount)] := fv3add(centerloc, scaleVec(vec, aRadius * scale));
    end;

    linch1 := mVertCount;
    aBranch.mRing0[inc_(ring0count)] := linch1;
    aBranch.mRing1[inc_(ring1count)] := linch1;
    mVert[inc_(mVertCount)] := fv3add(centerloc, scaleVec(tangent, -aRadius * scale));

    for i := segments div 2 + 1 to segments - 1 do
    begin
      vec := vecAxisAngle(tangent, axis1, segmentAngle * i);
      aBranch.mRing0[inc_(ring0count)] := mVertCount;
      aBranch.mRing1[inc_(ring1count)] := mVertCount;
      mVert[inc_(mVertCount)] := (fv3add(centerloc, scaleVec(vec, aRadius * scale)));
    end;

    aBranch.mRing1[inc_(ring1count)] := linch0;
    aBranch.mRing2[inc_(ring2count)] := linch1;
    start := mVertCount - 1;

    for i := 1 to segments div 2 - 1 do
    begin
      vec := vecAxisAngle(tangent, axis3, segmentAngle * i);
      aBranch.mRing1[inc_(ring1count)] := start + i;
      aBranch.mRing2[inc_(ring2count)] := start + (segments div 2 - i);
      mVert[inc_(mVertCount)] := fv3add(centerloc, scaleVec(vec, aRadius * scale));
    end;

    //child radius is related to the brans direction and the length of the branch

    radius0 := aRadius * mProperties.mRadiusFalloffRate;
    radius1 := aRadius * mProperties.mRadiusFalloffRate;

    if aBranch.mChild0.mTrunktype <> 0 then
      radius0 := aRadius * mProperties.mTaperRate;

    createForks(aBranch.mChild0, radius0);
    createForks(aBranch.mChild1, radius1);
  end
  else
  begin
    //add points for the ends of braches
    aBranch.mEnd := mVertCount;
    mVert[inc_(mVertCount)] := aBranch.mHead;
  end;
end;

procedure tree_t.fixUVs;
var
  badverttable: array of integer;
  i, j, idx: integer;
  badverts: integer;
  found: boolean;
begin
  // There'll never be more than 50% bad vertices
  SetLength(badverttable, mVertCount div 2);
  badverts := 0;

  // step 1: find bad verts
  // - If edge's U coordinate delta is over 0.5, texture has wrapped around.
  // - The vertex that has zero U is the wrong one
  // - Care needs to be taken not to tag bad vertex more than once.

  for i := 0 to mFaceCount - 1 do
  begin
    // x/y edges (vertex 0 and 1)
    if (abs(mUV[mFace[i].x].u - mUV[mFace[i].y].u) > 0.5) and ((mUV[mFace[i].x].u = 0.0) or (mUV[mFace[i].y].u = 0.0)) then
    begin
      found := False;
      for j := 0 to badverts - 1 do
      begin
        if (badverttable[j] = mFace[i].y) and (mUV[mFace[i].y].u = 0.0) then
            found := True;
        if (badverttable[j] = mFace[i].x) and (mUV[mFace[i].x].u = 0.0) then
            found := True;
      end;
      if not found then
      begin
        if mUV[mFace[i].x].u = 0.0 then
          badverttable[badverts] := mFace[i].x;
        if mUV[mFace[i].y].u = 0.0 then
          badverttable[badverts] := mFace[i].y;
        inc(badverts);
      end;
    end;

    // x/z edges (vertex 0 and 2)
    if (abs(mUV[mFace[i].x].u - mUV[mFace[i].z].u) > 0.5) and ((mUV[mFace[i].x].u = 0.0) or (mUV[mFace[i].z].u = 0.0)) then
    begin
      found := False;
      for j := 0 to badverts - 1 do
      begin
        if (badverttable[j] = mFace[i].z) and (mUV[mFace[i].z].u = 0.0) then
          found := True;
        if (badverttable[j] = mFace[i].x) and (mUV[mFace[i].x].u = 0.0) then
          found := True;
      end;
      if not found then
      begin
        if mUV[mFace[i].x].u = 0.0 then
          badverttable[badverts] := mFace[i].x;
        if mUV[mFace[i].z].u = 0.0 then
          badverttable[badverts] := mFace[i].z;
        inc(badverts);
      end;
    end;

    // y/z edges (vertex 1 and 2)
    if (abs(mUV[mFace[i].y].u - mUV[mFace[i].z].u) > 0.5) and ((mUV[mFace[i].y].u = 0.0) or (mUV[mFace[i].z].u = 0.0)) then
    begin
      found := False;
      for j := 0 to badverts - 1 do
      begin
        if (badverttable[j] = mFace[i].z) and (mUV[mFace[i].z].u = 0.0) then
          found := True;
        if (badverttable[j] = mFace[i].y) and (mUV[mFace[i].y].u = 0.0) then
          found := True;
      end;
      if not found then
      begin
        if mUV[mFace[i].y].u = 0.0 then
          badverttable[badverts] := mFace[i].y;
        if mUV[mFace[i].z].u = 0.0 then
          badverttable[badverts] := mFace[i].z;
        inc(badverts);
      end;
    end;
  end;

  // step 2: allocate more space for our new duplicate verts

  SetLength(mVert, mVertCount + badverts);
  SetLength(mNormal, mVertCount + badverts);
  SetLength(mUV, mVertCount + badverts);

  // step 3: populate duplicate verts - otherwise identical except for U=1 instead of 0

  for i := 0 to badverts - 1 do
  begin
    mVert[mVertCount + i] := mVert[badverttable[i]];
    mNormal[mVertCount + i] := mNormal[badverttable[i]];
    mUV[mVertCount + i] := mUV[badverttable[i]];
    mUV[mVertCount + i].u := 1.0;
  end;

  // step 4: fix faces

  for i := 0 to mFaceCount - 1 do
  begin
    // x/y edges (vertex 0 and 1)
    if (abs(mUV[mFace[i].x].u - mUV[mFace[i].y].u) > 0.5) and ((mUV[mFace[i].x].u = 0.0) or (mUV[mFace[i].y].u = 0.0)) then
    begin
      idx := 0;
      for j := 0 to badverts - 1 do
      begin
        if (badverttable[j] = mFace[i].y) and (mUV[mFace[i].y].u = 0.0) then
          idx := j;
        if (badverttable[j] = mFace[i].x) and (mUV[mFace[i].x].u = 0.0) then
          idx := j;
      end;
      if mUV[mFace[i].y].u = 0.0 then
        mFace[i].y := mVertCount + idx;
      if mUV[mFace[i].x].u = 0.0 then
        mFace[i].x := mVertCount + idx;
    end;

    // x/z edges (vertex 0 and 2)
    if (abs(mUV[mFace[i].x].u - mUV[mFace[i].z].u) > 0.5) and ((mUV[mFace[i].x].u = 0.0) or (mUV[mFace[i].z].u = 0.0)) then
    begin
      idx := 0;
      for j := 0 to badverts - 1 do
      begin
        if (badverttable[j] = mFace[i].z) and (mUV[mFace[i].z].u = 0.0) then
          idx := j;
        if (badverttable[j] = mFace[i].x) and (mUV[mFace[i].x].u = 0.0) then
          idx := j;
      end;
      if mUV[mFace[i].x].u = 0.0 then
        mFace[i].x := mVertCount + idx;
      if mUV[mFace[i].z].u = 0.0 then
        mFace[i].z := mVertCount + idx;
    end;

    // y/z edges (vertex 1 and 2)
    if (abs(mUV[mFace[i].y].u - mUV[mFace[i].z].u) > 0.5) and ((mUV[mFace[i].y].u = 0.0) or (mUV[mFace[i].z].u = 0.0)) then
    begin
      idx := 0;
      for j := 0 to badverts - 1 do
      begin
        if (badverttable[j] = mFace[i].z) and (mUV[mFace[i].z].u = 0.0) then
          idx := j;
        if (badverttable[j] = mFace[i].y) and (mUV[mFace[i].y].u = 0.0) then
          idx := j;
      end;
      if mUV[mFace[i].y].u = 0.0 then
        mFace[i].y := mVertCount + idx;
      if mUV[mFace[i].z].u = 0.0 then
        mFace[i].z := mVertCount + idx;
    end;
  end;

  // step 5: update vert count
  inc(mVertCount, badverts);

  // and cleanup
  SetLength(badverttable, 0);
end;

end.
