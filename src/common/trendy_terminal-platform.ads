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

with Ada.Strings.Unbounded;

-- Trendy Terminal defines a known environment in which to perform input/output.
-- Failure to meet these requirements results in an failed initialization.
--
-- The requirements:
-- 1. UTF-8
-- 2. VT100 terminal escape sequences.
--
-- The base package provides platform-specific environment setup, and the basic
-- read/write commands on top of which to build functionality.
package Trendy_Terminal.Platform is
    package ASU renames Ada.Strings.Unbounded;

    -- Initializes and captures the original settings for the terminal so they can
    -- be restored when the system is shutdown.
    function Init return Boolean;

    -- Restores the system to the conditions prior to calling `Init`.
    procedure Shutdown;

    type Input_Setting is (Echo, Line_Input, Signals_As_Input);
    type Output_Setting is (Escape_Sequences);

    procedure Set (Setting : Input_Setting; Enabled : Boolean);
    procedure Set (Setting : Output_Setting; Enabled : Boolean);

    -- These are platform-specific terminal read/write functions to avoid
    -- messing with Ada standard library internals such as current column.
    -- This avoids spurious formatting and other implementation quirks of
    -- those libraries.
    procedure Put (C : Character);
    procedure Put (S : String);

    function Get_Input return String;

    function End_Of_Line return String;

    procedure Print_Configuration;

end Trendy_Terminal.Platform;
