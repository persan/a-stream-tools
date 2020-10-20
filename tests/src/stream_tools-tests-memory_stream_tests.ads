with AUnit; use AUnit;
with AUnit.Test_Cases; use AUnit.Test_Cases;

package Stream_Tools.Tests.Memory_Stream_Tests is
   type Memory_Stream_Test is new Test_Cases.Test_Case with null record;

   overriding procedure Register_Tests (T : in out Memory_Stream_Test);
   --  Register routines to be run

   overriding function Name (T : Memory_Stream_Test) return Message_String;
   --  Provide name identifying the test case

   --  Test Routines:
   procedure Test_Simple (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Read_Past_Eof_Raise (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Read_Element_Array_1 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Read_Element_Array_2 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Read_Element_Array_3 (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Size (T : in out Test_Cases.Test_Case'Class);

   procedure Test_As_String (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Dump (T : in out Test_Cases.Test_Case'Class);
   procedure Test_As_Source (T : in out Test_Cases.Test_Case'Class);

end Stream_Tools.Tests.Memory_Stream_Tests;
