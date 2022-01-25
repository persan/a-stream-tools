with AUnit; use AUnit;
with AUnit.Test_Cases;
with Stream_Tools.Generic_Classwide_Ring;
package Stream_Tools.Tests.Generic_Classwide_Ring  is
   type A is tagged record
      Num : Integer;
   end record;

   type AB is new A with record
      Data_B : String (1 .. 5) := (others => '-');
   end record;

   type AA is new A with record
      Data_AA : String (1 .. 5) := (others => '#');
   end record;

   type ABA is new AB with record
      Data_BA : String (1 .. 5) := (others => '+');
   end record;

   package Ring is new Stream_Tools.Generic_Classwide_Ring (A);

   type Test_Case is new Test_Cases.Test_Case with  record
      Buffer_64 : aliased Ring.Ring_Buffer (128, 128);
   end record;

   overriding procedure Register_Tests (T : in out Test_Case);
   --  Register routines to be run

   overriding function Name (T : Test_Case) return Message_String;
   --  Provide name identifying the test case

   --  Test Routines:
   procedure Test_Output (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Input (T : in out Test_Cases.Test_Case'Class);
   procedure Test_Input_Output (T : in out Test_Cases.Test_Case'Class);

end Stream_Tools.Tests.Generic_Classwide_Ring;
