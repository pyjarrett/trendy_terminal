with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Interfaces.C;

package body Trendy_Terminal.Input is
    package ASU renames Ada.Strings.Unbounded;

    type HKL is new Interfaces.C.ptrdiff_t;
    type SHORT is new Interfaces.C.unsigned_short;
    type DWORD is new Interfaces.C.unsigned_long;

    function GetKeyboardLayout(ThreadId : DWORD) return HKL;
    function GetCurrentThreadId return DWORD;
    --  function VkKeyScanExA(C : Character; A_HKL : HKL) return SHORT;
    function VkKeyScanExA(I : Interfaces.C.int; A_HKL : HKL) return SHORT;

    pragma Import (Stdcall, GetKeyboardLayout, "GetKeyboardLayout");
    pragma Import (Stdcall, GetCurrentThreadId, "GetCurrentThreadId");
    pragma Import (Stdcall, VkKeyScanExA, "VkKeyScanExA");

    function getchar return Interfaces.C.int;
    pragma Import (C, getchar);

    -- Readline essentials
    -- https://www.gnu.org/software/bash/manual/html_node/Readline-Interaction.html
    -- https://github.com/AmokHuginnsson/replxx/blob/master/examples/cxx-api.cxx
    function Get_Line return String is
        Current_Line : ASU.Unbounded_String;
        Next_Input   : Interfaces.C.int;
        Key          : SHORT;
        Key_Enter    : constant := 13;
        use all type Interfaces.C.int;
    begin
        loop
            Next_Input := getchar;
            if Next_Input = Key_Enter then
                Ada.Text_IO.Put_Line ("exiting");
                exit;
            end if;
            Key := VkKeyScanExA (Next_Input, GetKeyboardLayout(GetCurrentThreadId));
            Ada.Text_IO.Put_Line ("Got key: " & Key'Image & "  " & Next_Input'Image);
        end loop;

        return ASU.To_String(Current_Line);
    end Get_Line;



end Trendy_Terminal.Input;
