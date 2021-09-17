with Ada.Finalization;

package Trendy_Terminal.Environments is

    -- RAII-based type to restored the terminal environment on exit.
    type Environment is tagged limited private;

    function Is_Available(Self : Environment) return Boolean;

private

    type Environment is new Ada.Finalization.Limited_Controlled with record
        Initialized : Boolean := False;
    end record;

    overriding
    procedure Initialize (Self : in out Environment);

    overriding
    procedure Finalize (Self: in out Environment);

end Trendy_Terminal.Environments;
