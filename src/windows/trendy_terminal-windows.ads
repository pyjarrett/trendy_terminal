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

with Ada.Unchecked_Conversion;
with Interfaces.C.Strings;

---------------------------------------------------------------------------
-- Win32 API functions
--
-- https://docs.microsoft.com/en-us/windows/console/classic-vs-vt
---------------------------------------------------------------------------
package Trendy_Terminal.Windows is
    type LONG is new Interfaces.C.long;
    type DWORD is new Interfaces.C.unsigned_long;
    type HANDLE is new Interfaces.C.ptrdiff_t;
    type LPDWORD is access all DWORD;
    type BOOL is new Interfaces.C.int;
    type UINT is new Interfaces.C.unsigned;

    function To_DWORD is new Ada.Unchecked_Conversion (LONG, DWORD);

    INVALID_HANDLE_VALUE : constant       := -1;
    STD_INPUT_HANDLE     : constant DWORD := To_DWORD (-10);
    STD_OUTPUT_HANDLE    : constant DWORD := To_DWORD (-11);
    STD_ERROR_HANDLE     : constant DWORD := To_DWORD (-12);

    function GetStdHandle (Std_Handle : DWORD) return HANDLE;
    function GetConsoleMode (H : HANDLE; Mode : LPDWORD) return BOOL;
    function SetConsoleMode (H : HANDLE; dwMode : DWORD) return BOOL;

    function GetLastError return DWORD;

    CP_UTF8 : constant := 65_001;

    function GetConsoleCP return UINT;
    function GetConsoleOutputCP return UINT;

    function SetConsoleCP (wCodePageID : UINT) return BOOL;
    function SetConsoleOutputCP (wCodePageID : UINT) return BOOL;

    pragma Import (Stdcall, GetStdHandle, "GetStdHandle");
    pragma Import (Stdcall, GetConsoleMode, "GetConsoleMode");
    pragma Import (Stdcall, SetConsoleMode, "SetConsoleMode");
    pragma Import (Stdcall, GetLastError, "GetLastError");

    pragma Import (Stdcall, GetConsoleCP, "GetConsoleCP");
    pragma Import (Stdcall, SetConsoleCP, "SetConsoleCP");
    pragma Import (Stdcall, GetConsoleOutputCP, "GetConsoleOutputCP");
    pragma Import (Stdcall, SetConsoleOutputCP, "SetConsoleOutputCP");

    type Console_Input_Flags is (
        ENABLE_PROCESSED_INPUT,
        ENABLE_LINE_INPUT,
        ENABLE_ECHO_INPUT,
        ENABLE_WINDOW_INPUT,
        ENABLE_MOUSE_INPUT,
        ENABLE_INSERT_MODE,
        ENABLE_QUICK_EDIT_MODE,
        ENABLE_EXTENDED_FLAGS,
        ENABLE_AUTO_POSITION,
        ENABLE_VIRTUAL_TERMINAL_INPUT);

    for Console_Input_Flags use (
        ENABLE_PROCESSED_INPUT        => 16#0001#,
        ENABLE_LINE_INPUT             => 16#0002#,
        ENABLE_ECHO_INPUT             => 16#0004#,
        ENABLE_WINDOW_INPUT           => 16#0008#,
        ENABLE_MOUSE_INPUT            => 16#0010#,
        ENABLE_INSERT_MODE            => 16#0020#,
        ENABLE_QUICK_EDIT_MODE        => 16#0040#,
        ENABLE_EXTENDED_FLAGS         => 16#0080#,
        ENABLE_AUTO_POSITION          => 16#0100#,
        ENABLE_VIRTUAL_TERMINAL_INPUT => 16#0200#);

    type Console_Output_Flags is (
        ENABLE_PROCESSED_OUTPUT,
        ENABLE_WRAP_AT_EOL_OUTPUT,
        ENABLE_VIRTUAL_TERMINAL_PROCESSING,
        DISABLE_NEWLINE_AUTO_RETURN,
        ENABLE_LVB_GRID_WORLDWIDE);

    for Console_Output_Flags use (
        ENABLE_PROCESSED_OUTPUT            => 16#0001#,
        ENABLE_WRAP_AT_EOL_OUTPUT          => 16#0002#,
        ENABLE_VIRTUAL_TERMINAL_PROCESSING => 16#0004#,
        DISABLE_NEWLINE_AUTO_RETURN        => 16#0008#,
        ENABLE_LVB_GRID_WORLDWIDE          => 16#0010#);

    pragma Warnings (Off, "bits of *unused");
    type Console_Input_Mode is array (Console_Input_Flags) of Boolean with
        Pack,
        Size => 32;
    type Console_Output_Mode is array (Console_Output_Flags) of Boolean with
        Pack,
        Size => 32;
    pragma Warnings (On, "bits of *unused");

    function To_Console_Input_Mode is new Ada.Unchecked_Conversion (DWORD, Console_Input_Mode);
    function To_Console_Output_Mode is new Ada.Unchecked_Conversion (DWORD, Console_Output_Mode);
    function To_DWORD is new Ada.Unchecked_Conversion (Console_Input_Mode, DWORD);
    function To_DWORD is new Ada.Unchecked_Conversion (Console_Output_Mode, DWORD);

    type LPVOID is new Interfaces.C.Strings.chars_ptr;
    type LPCVOID is new Interfaces.C.Strings.chars_ptr;
    type LPOVERLAPPED is new Interfaces.C.ptrdiff_t;

    function WriteConsoleA(hFile : HANDLE; lpBuffer : LPCVOID; BytesToWrite : DWORD;
                        NumBytesWritten : LPDWORD; Overlapped : LPOVERLAPPED) return BOOL;
    pragma Import (Stdcall, WriteConsoleA, "WriteConsoleA");

    function ReadConsoleA (I               : HANDLE;
                           Buffer          : LPVOID;
                           Buffer_Size     : DWORD;
                           Characters_Read : LPDWORD;
                           Console_Control : Interfaces.C.ptrdiff_t) return BOOL;
    pragma Import (Stdcall, ReadConsoleA, "ReadConsoleA");

end Trendy_Terminal.Windows;
