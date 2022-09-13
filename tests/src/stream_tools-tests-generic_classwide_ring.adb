pragma Ada_2012;
with AUnit.Assertions;
with GNAT.Source_Info;
package body Stream_Tools.Tests.Generic_Classwide_Ring is
   use AUnit.Assertions;
   ----------
-- Name --
----------
   Unit_Name : constant String := GNAT.Source_Info.Enclosing_Entity;

   overriding function Name (T : Test_Case) return Message_String is
      pragma Unreferenced (T);
   begin
      return Format (Unit_Name);
   end Name;

   -----------------
   -- Test_Simple --
   -----------------

   procedure Test_Output (T : in out Test_Cases.Test_Case'Class) is
      Tc : Test_Case renames Test_Case (T);
   begin
      Tc.Buffer_64.Output (AB'(Num => 1, others => <>));
      Tc.Buffer_64.Output (AB'(Num => 2, others => <>));
   end Test_Output;

   procedure Test_Input (T : in out Test_Cases.Test_Case'Class) is
      Tc     : Test_Case renames Test_Case (T);
      Index  : Integer := 0;
      Counter : Integer := 0;
   begin
      for I of Tc.Buffer_64 loop
         Assert (Index < I.Num, "Invalid value I.Num=>" & I.Num'Img & ",Counter=>" & Counter'Img);
         Index := I.Num;
         Counter := Counter + 1;
      end loop;
      Assert (Counter > 0, "No data in buffer");
   end Test_Input;

   procedure Test_Input_Output (T : in out Test_Cases.Test_Case'Class) is
      Tc : Test_Case renames Test_Case (T);
      D  : constant A'CLASS := AB'(Num => 1, others => <>);
   begin
      Tc.Buffer_64.Reset (Hard => True);
      Tc.Buffer_64.Output (D);
      declare
         D2 : constant A'Class := Tc.Buffer_64.Input;
      begin
         AUnit.Assertions.Assert (D2 = D, "Wrong data");
      end;
   end Test_Input_Output;
   procedure Test_Output_Large (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Output_Large (T : in out Test_Cases.Test_Case'Class) is
      Tc : Test_Case renames Test_Case (T);
   begin
      for Ix in 0 .. 10 loop
         Tc.Buffer_64.Output (AB'(Num => Ix, others => <>));
      end loop;
   end Test_Output_Large;
   --------------------
   -- Register_Tests --
   --------------------
   overriding procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases;
   begin
      Registration.Register_Routine (T, Test_Output'Access, "Test_Output");
      Registration.Register_Routine (T, Test_Input'Access, "Test_Input");
      Registration.Register_Routine (T, Test_Input_Output'Access, "Test_Input_Output");
      Registration.Register_Routine (T, Test_Output_Large'Access, "Test_Output_Large");

   end Register_Tests;

end Stream_Tools.Tests.Generic_Classwide_Ring;
