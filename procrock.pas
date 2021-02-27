//------------------------------------------------------------------------------
//
//  DOOMROCK: Doom Rock Sprite Generator
//  Copyright (C) 2021 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Procedural rock generator
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/doom-rock/
//------------------------------------------------------------------------------


unit procrock;

interface

type
  fvec5_t = record
    x, y, z, u, v: single;
    ring: integer;
  end;
  fvec5_p = ^fvec5_t;
  fvec5_a = array[0..$FFFF] of fvec5_t;
  fvec5_pa = ^fvec5_a;

  ivec3_t = record
    x, y, z: integer;
    topring: integer;
    bottomring: integer;
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
    mRockSteps: integer;
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
    mUScale: single; // U texture coordinate scale
    mVScale: single; // V texture coordinate scale
    mXScale: single; // X axis scale
    mYScale: single; // Y axis scale
    mZScale: single; // Z axis scale
    mXDeformFactor: single; // X axis deformation
    mYDeformFactor: single; // Y axis deformation
    mZDeformFactor: single; // Z axis deformation
    mRDeformFactor: single; // Radius deformation
    mNumRings: integer; // Number of rings
    mNumSegments: integer; // Number of segments
    mXOffset: single; // X axis offset
    mZOffset: single; // Z axis offset
    mPitRate: single; // Pit rate
    mPitScaleMin: single; // Pit scale minimum
    mPitScaleMax: single; // Pit scale miximum
    mGroundLevelFactor: single; // Close to ground level control
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
      aRockSteps: integer;
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


  rock_t = class
  protected
    procedure init;
    function AddVert(const x, y, z, u, v: single): integer;
    procedure generate_sphere;
  public
    mProperties: properties_t;
    mVertCount: integer;
    mFaceCount: integer;

    mVert: array of fvec5_t;
    mFace: array of ivec3_t;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure generate;
  end;

implementation

uses
  Math;

const
  M_PI = 3.1415926535897932384626433832795;

function makefvec3(const x, y, z, u, v: single): fvec5_t;
begin
  result.x := x;
  result.y := y;
  result.z := z;
  result.u := u;
  result.v := v;
end;

function makeivec3(const x, y, z: integer): ivec3_t;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

function fv3length(const a: fvec5_t): single;
begin
  result := sqrt(a.x * a.x + a.y * a.y + a.z * a.z);
end;

function fv3normalize(const a: fvec5_t): fvec5_t;
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

function scaleVec(const a: fvec5_t; const b: single): fvec5_t;
begin
  result := a;
  result.x := a.x * b;
  result.y := a.y * b;
  result.z := a.z * b;
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
  aRockSteps: integer;
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
  mRockSteps := aRockSteps;
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
  mRockSteps := 5;
  mTaperRate := 0.947;
  mRadiusFalloffRate := 0.73;
  mTwistRate := 3.02;
  mTrunkLength := 2.4;

  mUScale := 1.0;
  mVScale := 1.0;
  mXScale := 1.0;
  mYScale := 1.0;
  mZScale := 1.0;
  mXDeformFactor := 0.0;
  mYDeformFactor := 0.0;
  mZDeformFactor := 0.0;
  mRDeformFactor := 0.0;
  mNumRings := 5;
  mNumSegments := 10;
  mXOffset := 0.0;
  mZOffset := 0.0;
  mPitRate := 0.0;
  mPitScaleMin := 1.0;
  mPitScaleMax := 1.0;
  mGroundLevelFactor := 1.0;
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

constructor rock_t.Create;
begin
  mProperties := properties_t.CreateDefault;

  SetLength(mVert, 0);
  SetLength(mFace, 0);

  mVertCount := 0;
  mFaceCount := 0;
end;

destructor rock_t.Destroy;
begin
  mProperties.Free;
  SetLength(mVert, 0);
  SetLength(mFace, 0);
end;

// Bigger values = better accuracy
const
  MAXRINGS = 32;
  MAXSEGMENTS = 32;

  EPSILON = 0.000001;

function rock_t.AddVert(const x, y, z, u, v: single): integer;
function rock_t.AddVert(const x, y, z, u, v: single; const ring: integer): integer;
var
  i: integer;
begin
  for i := 0 to mVertCount - 1 do
    if abs(x - mVert[i].x) < EPSILON then
      if abs(y - mVert[i].y) < EPSILON then
        if abs(z - mVert[i].z) < EPSILON then
        begin
          Result := i;
          Exit;
        end;

  Result := mVertCount;
  inc(mVertCount);
  SetLength(mVert, mVertCount);
  mVert[Result].x := x;
  mVert[Result].y := y;
  mVert[Result].z := z;
  mVert[Result].u := u;
  mVert[Result].v := v;
  mVert[Result].ring := ring;
end;

function min3i(const x1, x2, x3: integer): integer;
begin
  Result := x1;
  if x2 < Result then
    Result := x2;
  if x3 < Result then
    Result := x3;
end;

function max3i(const x1, x2, x3: integer): integer;
begin
  Result := x1;
  if x2 > Result then
    Result := x2;
  if x3 > Result then
    Result := x3;
end;

procedure rock_t.generate_hemisphere;
var
  ring, seg: integer;
  fDeltaRingAngle: single;
  fDeltaSegAngle: single;
  ss, sc: single;
  r0, r1: single;
  x0, x1: single;
  y0, y1: single;
  z0, z1: single;
  idx: integer;
  vec: fvec5_t;
  A: array[0..MAXRINGS * (MAXSEGMENTS + 1) - 1] of integer;
  numrings: integer;
  numsegments: integer;
begin
  mProperties.mRseed := mProperties.mSeed;

  numrings := mProperties.mNumRings * 2;
  numsegments := mProperties.mNumSegments;

  mVertCount := 0;
  SetLength(mVert, mVertCount);

  fDeltaRingAngle := pi / numrings;
  fDeltaSegAngle  := 2 * pi / numsegments;

  idx := 0;

  // Generate the group of rings for the hemishere
  for ring := 0 to numrings div 2 - 1 do
  begin
    r0 := Sin(ring * fDeltaRingAngle);
    y0 := Cos(ring * fDeltaRingAngle);
    r1 := Sin((ring + 1) * fDeltaRingAngle);
    y1 := Cos((ring + 1) * fDeltaRingAngle);

    // Generate the group of segments for the current ring
    for seg := 0 to numsegments do
    begin
      ss := Sin(seg * fDeltaSegAngle);
      sc := Cos(seg * fDeltaSegAngle);
      x0 := r0 * ss;
      z0 := r0 * sc;
      x1 := r1 * ss;
      z1 := r1 * sc;

      vec.x := x0;
      vec.y := y0;
      vec.z := z0;
      vec.u := -x0 / 2 + 0.5;
      vec.v := -z0 / 2 + 0.5;
      A[idx] := AddVert(vec.x, vec.y, vec.z, vec.u, vec.v);
      A[idx] := AddVert(vec.x, vec.y, vec.z, vec.u, vec.v, ring);
      inc(idx);

      vec.x := x1;
      vec.y := y1;
      vec.z := z1;
      vec.u := -x1 / 2 + 0.5;
      vec.v := -z1 / 2 + 0.5;
      A[idx] := AddVert(vec.x, vec.y, vec.z, vec.u, vec.v);
      A[idx] := AddVert(vec.x, vec.y, vec.z, vec.u, vec.v, ring + 1);
      inc(idx);
    end;
  end;

{  for idx := 0 to GetArrayLength(A) - 1 do
  begin
    A[idx].x := (A[idx].x * radius) + x;
    A[idx].y := (A[idx].y * radius) + y;
    A[idx].z := (A[idx].z * radius) + z;
  end;}

  mFaceCount := numrings * (numsegments + 1) - 2;
  SetLength(mFace, mFaceCount);

  for idx := 0 to mFaceCount - 1 do
  begin
    mFace[idx].x := A[idx];
    mFace[idx].y := A[idx + 1];
    mFace[idx].z := A[idx + 2];
    mFace[idx].topring := min3i(mVert[idx].ring, mVert[idx + 1].ring, mVert[idx + 2].ring);
    mFace[idx].bottomring := max3i(mVert[idx].ring, mVert[idx + 1].ring, mVert[idx + 2].ring);
  end;

  for idx := 0 to mVertCount - 1 do
  begin
    mVert[idx].x := mVert[idx].x + mProperties.random(0) / 5;
    if abs(mVert[idx].y) > EPSILON then
      mVert[idx].y := mVert[idx].y + mProperties.random(0) / 5;
    mVert[idx].z := mVert[idx].z + mProperties.random(0) / 5;
//    mVert[idx].u := -mVert[idx].x / 2 + 0.5;
//    mVert[idx].v := -mVert[idx].z / 2 + 0.5;
    mVert[idx] := scaleVec(mVert[idx], 1.1 - mProperties.random(0) * 0.2);
  end;
end;

procedure rock_t.generate;
begin
  generate_sphere;
end;

procedure rock_t.init;
begin
  SetLength(mVert, 0);
  SetLength(mFace, 0);

  mVertCount := 0;
  mFaceCount := 0;
end;


end.
