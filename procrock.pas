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
    ring, seg: integer;
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

// Bigger values = better accuracy
const
  MAXRINGS = 32;
  MAXSEGMENTS = 32;

type
  UVmatrixLookUp_t = array[0..MAXRINGS - 1, 0..MAXSEGMENTS] of integer;

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
    mXCareen: single; // X axis careen
    mYCareen: single; // X axis careen
    mZCareen: single; // X axis careen
    mRecalcUV: boolean; // Recalculate UV
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
    uvmatrixlookup: UVmatrixLookUp_t;
    procedure init;
    function AddVert(const x, y, z, u, v: single; const ring, seg: integer): integer;
    procedure generate_hemisphere;
    procedure fix_uvscale;
    procedure apply_uvscale;
    procedure apply_xyzdeformation;
    procedure apply_xyzcareen;
    procedure apply_rdeformation;
    procedure apply_xzoffsets;
    procedure apply_xyzscale;
    procedure apply_pits;
    procedure apply_groundlevelfactor;
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

function fv5dist(const a1, a2: fvec5_t): single;
var
  dx, dy, dz: single;
begin
  dx := a1.x - a2.x;
  dy := a1.y - a2.y;
  dz := a1.z - a2.z;
  result := sqrt(dx * dx + dy * dy + dz * dz);
end;

function fv5length(const a: fvec5_t): single;
begin
  result := sqrt(a.x * a.x + a.y * a.y + a.z * a.z);
end;

function fv3normalize(const a: fvec5_t): fvec5_t;
var
  l: single;
begin
  l := fv5length(a);
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
  mXCareen := 0.0;
  mYCareen := 0.0;
  mZCareen := 0.0;
  mRecalcUV := True;
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

const
  EPSILON = 0.000001;

function rock_t.AddVert(const x, y, z, u, v: single; const ring, seg: integer): integer;
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
  mVert[Result].seg := seg;
  uvmatrixlookup[ring, seg] := Result;
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

  for ring := 0 to numrings div 2 - 1 do
    for seg := 0 to numsegments do
      uvmatrixlookup[ring, seg] := 0;

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
      vec.u := -x0 / 2;// + 0.5;
      vec.v := -z0 / 2;// + 0.5;
      A[idx] := AddVert(vec.x, vec.y, vec.z, vec.u, vec.v, ring, seg);
      inc(idx);

      vec.x := x1;
      vec.y := y1;
      vec.z := z1;
      vec.u := -x1 / 2;// + 0.5;
      vec.v := -z1 / 2;// + 0.5;
      A[idx] := AddVert(vec.x, vec.y, vec.z, vec.u, vec.v, ring + 1, seg);
      inc(idx);
    end;
  end;

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
end;

procedure rock_t.fix_uvscale;
var
  ring, seg: integer;
  numrings: integer;
  numsegments: integer;
  LENGTHS: array[0..MAXRINGS - 1] of single;
  len, totallen: single;
  v1, v2: integer;
  theta: single;
begin
  numrings := mProperties.mNumRings * 2;
  numsegments := mProperties.mNumSegments;

  LENGTHS[0] := 0.0;
  for seg := 0 to numsegments do
  begin
    totallen := 0.0;
    for ring := 1 to numrings div 2 do
    begin
      v1 := uvmatrixlookup[ring, seg];
      v2 := uvmatrixlookup[ring - 1, seg];
      len := fv5dist(mVert[v1], mVert[v2]);
      totallen := totallen + len;
      LENGTHS[ring] := totallen;
    end;
    for ring := 1 to numrings div 2 do
    begin
      v1 := uvmatrixlookup[ring, seg];
      theta := ArcTan2(mVert[v1].z, mVert[v1].x);
      mVert[v1].u := sin(theta) * (LENGTHS[ring] / totallen) + 0.5;
      mVert[v1].v := cos(theta) * (LENGTHS[ring] / totallen) + 0.5;
    end;
  end;
  mVert[uvmatrixlookup[0, 0]].u := 0.5;
  mVert[uvmatrixlookup[0, 0]].v := 0.5;
end;


procedure rock_t.apply_uvscale;
var
  i: integer;
  scale: single;
begin
  scale := mProperties.mUScale;
  if scale <> 1.0 then
    for i := 0 to mVertCount - 1 do
      mVert[i].u := (mVert[i].u - 0.5) * scale + 0.5;

  scale := mProperties.mVScale;
  if scale <> 1.0 then
    for i := 0 to mVertCount - 1 do
      mVert[i].v := (mVert[i].v - 0.5) * scale + 0.5;
end;

procedure rock_t.apply_xyzdeformation;
var
  i: integer;
  factor: single;
begin
  factor := mProperties.mXDeformFactor;
  if factor <> 0.0 then
    for i := 0 to mVertCount - 1 do
    begin
      if mProperties.random(0) < 0.5 then
        mVert[i].x := mVert[i].x - mProperties.random(0) * factor
      else
        mVert[i].x := mVert[i].x + mProperties.random(0) * factor;
    end;

  factor := mProperties.mYDeformFactor;
  if factor <> 0.0 then
    for i := 0 to mVertCount - 1 do
      if abs(mVert[i].y) > EPSILON then
      begin
        if mProperties.random(0) < 0.5 then
          mVert[i].y := mVert[i].y - mProperties.random(0) * factor
        else
          mVert[i].y := mVert[i].y + mProperties.random(0) * factor;
        if mVert[i].y < 0.0 then
          mVert[i].y := 0.0;
      end;

  factor := mProperties.mZDeformFactor;
  if factor <> 0.0 then
    for i := 0 to mVertCount - 1 do
    begin
      if mProperties.random(0) < 0.5 then
        mVert[i].z := mVert[i].z - mProperties.random(0) * factor
      else
        mVert[i].z := mVert[i].z + mProperties.random(0) * factor;
    end;
end;

procedure rock_t.apply_xyzcareen;
var
  i: integer;
  factor: single;
begin
  factor := mProperties.mXCareen;
  if factor <> 0.0 then
    for i := 0 to mVertCount - 1 do
      mVert[i].x := mVert[i].x + mProperties.random(0) * factor;

  factor := mProperties.mYCareen;
  if factor <> 0.0 then
    for i := 0 to mVertCount - 1 do
      if abs(mVert[i].y) > EPSILON then
      begin
        mVert[i].y := mVert[i].y + mProperties.random(0) * factor;
        if mVert[i].y < 0.0 then
          mVert[i].y := 0.0;
      end;

  factor := mProperties.mZCareen;
  if factor <> 0.0 then
    for i := 0 to mVertCount - 1 do
      mVert[i].z := mVert[i].z + mProperties.random(0) * factor;
end;

procedure rock_t.apply_rdeformation;
var
  i: integer;
  factor: single;
  f: single;
begin
  factor := mProperties.mRDeformFactor;
  if factor <> 0.0 then
  begin
    f := 1.0 + factor;
    for i := 0 to mVertCount - 1 do
      mVert[i] := scaleVec(mVert[i], f - mProperties.random(0) * factor);
  end;
end;

procedure rock_t.apply_xzoffsets;
var
  i: integer;
  offset: single;
begin
  offset := mProperties.mXOffset;
  if offset <> 0.0 then
    for i := 0 to mVertCount - 1 do
      mVert[i].x := mVert[i].x + offset;

  offset := mProperties.mZOffset;
  if offset <> 0.0 then
    for i := 0 to mVertCount - 1 do
      mVert[i].z := mVert[i].z + offset;
end;

procedure rock_t.apply_xyzscale;
var
  i: integer;
  scale: single;
begin
  scale := mProperties.mXScale;
  if scale <> 1.0 then
    for i := 0 to mVertCount - 1 do
      mVert[i].x := mVert[i].x * scale;

  scale := mProperties.mYScale;
  if scale <> 1.0 then
    for i := 0 to mVertCount - 1 do
      mVert[i].y := mVert[i].y * scale;

  scale := mProperties.mZScale;
  if scale <> 1.0 then
    for i := 0 to mVertCount - 1 do
      mVert[i].z := mVert[i].z * scale;
end;

procedure rock_t.apply_pits;
begin
end;

procedure rock_t.apply_groundlevelfactor;
begin
end;

procedure rock_t.generate;
begin
  generate_hemisphere;
  fix_uvscale;
  apply_uvscale;
  apply_xyzdeformation;
  apply_xyzcareen;
  apply_rdeformation;
  apply_xzoffsets;
  apply_xyzscale;
  apply_pits;
  apply_groundlevelfactor;
  if mProperties.mRecalcUV then
  begin
    fix_uvscale;
    apply_uvscale;
  end;
end;

procedure rock_t.init;
begin
  SetLength(mVert, 0);
  SetLength(mFace, 0);

  mVertCount := 0;
  mFaceCount := 0;
end;


end.
