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
package Trendy_Terminal is
    package ASU renames Ada.Strings.Unbounded;

    -- Initializes and captures the original settings for the terminal so they can
    -- be restored when the system is shutdown.
    function Init return Boolean;

    -- Restores the system to the conditions prior to calling `Init`.
    procedure Shutdown;

    type Input_Setting is (Echo, Line_Input);
    type Output_Setting is (Escape_Sequences);

    procedure Set (Setting : Input_Setting; Enabled : Boolean);
    procedure Set (Setting : Output_Setting; Enabled : Boolean);

    -- These are platform-specific terminal read/write functions to avoid
    -- messing with Ada standard library internals such as current column.
    -- This avoids spurious formatting and other implementation quirks of
    -- those libraries.
    procedure Put (C : Character);
    procedure Put (S : String);
    procedure Put (S : ASU.Unbounded_String);
    procedure Put_Line (S : String);
    procedure Put_Line (S : ASU.Unbounded_String);

    procedure Clear_Line;
    procedure New_Line (Num_Lines : Positive);
    procedure Set_Col (Column : Positive);

    type Format_Function is access function (S : String) return String;
    type Completion_Function is access function (S: String) return String;

    function Get_Input return String;
    function Get_Line (Format_Fn     : Format_Function := null;
                       Completion_Fn : Completion_Function := null) return String;

    -- A debug version of Get_Line for learning how to write an appropriate
    -- interface and callback system for Get_Line.
    function Debug_Get_Line (Format_Fn     : Format_Function := null;
                             Completion_Fn : Completion_Function := null;
                             Debug_Fn      : Format_Function := null) return String;

    procedure Clear_Input_Buffer;

    -- A position on screen.  Due to scrolling it is possible for these values
    -- to be negative.
    type Cursor_Position is record
        Row : Integer;
        Col : Integer;
    end record;

end Trendy_Terminal;
