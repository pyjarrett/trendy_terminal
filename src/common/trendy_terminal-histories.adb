-------------------------------------------------------------------------------
-- Copyright 2021, The Trendy Terminal Developers (see AUTHORS file)

-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at

--     http://www.apache.org/licenses/LICENSE-2.0

-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

package body Trendy_Terminal.Histories is

    package ASU renames Ada.Strings.Unbounded;

    procedure Add (H : in out History; Input : String) is
    begin
        H.Entries.Prepend (ASU.To_Unbounded_String (Input));
    end Add;

    procedure Set_Max_Entries (H : in out History; Count : Positive) is
    begin
        H.Max_Entries := Count;
    end Set_Max_Entries;

    function Num_Entries (H : History) return Natural is
    begin
        return Natural (H.Entries.Length);
    end Num_Entries;

    function Get_Entry (H : History; Index : Positive) return String is
    begin
        return ASU.To_String (H.Entries (Index));
    end Get_Entry;

    function Completions_Matching (H : History; Incomplete : String) return Lines.Line_Vectors.Vector is
        Result : Lines.Line_Vectors.Vector;
    begin
        for Each of H.Entries loop
            if Ada.Strings.Fixed.Index (Source => Incomplete, Pattern => ASU.To_String (Each)) = 0 then
                Result.Append (Lines.Make (Each, ASU.Length (Each) + 1));
            end if;
        end loop;
        return Result;
    end Completions_Matching;

end Trendy_Terminal.Histories;
