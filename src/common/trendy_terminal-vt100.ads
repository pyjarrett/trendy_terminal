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

-- Platform agnostic VT100 commands.
--
-- ECMA-48 (ISO 6429)
-- C0 (7-bit) control codes
-- C1 (8-bit) control codes
-- CSI = Control Sequence Introducer
-- https://vt100.net/
-- https://vt100.net/docs/vt510-rm/chapter4.html
-- https://en.wiktionary.org/wiki/Appendix:Control_characters
-- https://mudhalla.net/tintin/info/vt100/
-- https://www.aivosto.com/articles/control-characters.html
-- https://invisible-island.net/xterm/ctlseqs/ctlseqs.html
-- https://docs.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
package Trendy_Terminal.VT100 is
    procedure Cursor_Left;
    procedure Cursor_Right;
    procedure Cursor_Down;
    procedure Cursor_Up;

    procedure Hide_Cursor;
    procedure Show_Cursor;

    procedure Scroll_Down;
    procedure Scroll_Up;

    procedure Cursor_Next_Line;

    procedure Erase;

    procedure Beginning_Of_Line;
    procedure Clear_Line;

    procedure Report_Cursor_Position;

    -- A position on screen.  Due to scrolling it is possible for these values
    -- to be negative.
    type Cursor_Position is record
        Row : Integer;
        Col : Integer;
    end record;

    procedure Set_Cursor_Position (C : Cursor_Position);
    function Get_Cursor_Position return Cursor_Position;

end Trendy_Terminal.VT100;
