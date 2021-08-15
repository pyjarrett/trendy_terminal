with Ada.Characters.Latin_1;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package body Trendy_Terminal.Input is

    -- type Command_Input is record
    --      Cursor_Pos : Positive := 1;
    --      Line       : Ada.Strings.Unbounded.Unbounded_String;
    --  end record;

    -- Readline essentials
    -- https://www.gnu.org/software/bash/manual/html_node/Readline-Interaction.html
    function Get_Line return String is
        package ASU renames Ada.Strings.Unbounded;
        Next_Input   : Character;
        Current_Line : ASU.Unbounded_String;
        use Ada.Characters;
    begin
        loop
            Ada.Text_IO.Get_Immediate (Next_Input);

            --  if Ada.Characters.Handling.Is_Control(Next_Input) then
            --      Ada.Text_IO.Put_Line ("Control character");
            --  end if;

            if Next_Input in Latin_1.CR | Latin_1.FF then
                exit;
            end if;

            -- These are CTRL + whatever character
            -- Find which character by adding 96 ("a").
            -- This doesn't work completely well, since "Enter" is CTRL-m.
            --
            --  if Character'Pos(Next_Input) < 32 then
            --      Next_Input := Character'Val(Character'Pos(Next_Input) + 96);
            --  end if;

            case Next_Input is
                when Latin_1.BS =>
                    if ASU.Length (Current_Line) > 0 then
                        Current_Line :=
                            ASU.Unbounded_Slice
                                (Current_Line, 1, ASU.Length (Current_Line) - 1);
                        Cursor_Left;
                        Erase;
                    end if;
                when Character'Val(0) | Character'Val(224) =>
                    -- https://stackoverflow.com/questions/10463201/getch-and-arrow-codes
                    Ada.Text_IO.Get_Immediate (Next_Input);

                    -- Specially handle extra controls here.
                    Ada.Text_IO.Put_Line ("Handling: " & Next_Input'Image);
                when others =>
                    ASU.Append (Current_Line, Next_Input);
            end case;
        end loop;

        return ASU.To_String(Current_Line);
    end Get_Line;

end Trendy_Terminal.Input;
