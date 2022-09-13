with Stream_Tools.Tests.BUG20220913.Eth.Messages;
with Stream_Tools.Tests.BUG20220913.Eth.Messages.Command_Jak;
with GNAT.Source_Info;

package body Stream_Tools.Tests.BUG20220913 is

   procedure Crash_GNAT23 (T : in out Test_Cases.Test_Case'Class);

   procedure Crash_GNAT23 (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      procedure Socket_Write_PI_Memory_Stream (Data : Stream_Tools.Tests.BUG20220913.Eth.Messages.Root_Message_Type'Class);

      procedure Socket_Write_PI_Memory_Stream (Data : Stream_Tools.Tests.BUG20220913.Eth.Messages.Root_Message_Type'Class) is
         S    : aliased PI_Memory_Stream;
      begin
         Stream_Tools.Tests.BUG20220913.Eth.Messages.Root_Message_Type'Class'Output (S'Access, Data);
         --       S.Dump;
      end Socket_Write_PI_Memory_Stream;

      Message        : aliased Stream_Tools.Tests.BUG20220913.Eth.Messages.Command_Jak.Command_Jak_Message;

   begin
      Socket_Write_PI_Memory_Stream (Message);
   end Crash_GNAT23;

   procedure  Test_Memory_Stream (T : in out Test_Cases.Test_Case'Class);

   procedure Test_Memory_Stream (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      procedure Write_Memory_Stream (Data : Stream_Tools.Tests.BUG20220913.Eth.Messages.Root_Message_Type'Class);

      procedure Write_Memory_Stream (Data : Stream_Tools.Tests.BUG20220913.Eth.Messages.Root_Message_Type'Class) is
         S      : aliased Memory_Streams.Memory_Stream;
         Buffer : array (1 .. 1024) of Character;
      begin
         S.Set_Address (Buffer'Address);
         S.Set_Length (Buffer'Length);
         S.Reset;
         Stream_Tools.Tests.BUG20220913.Eth.Messages.Root_Message_Type'Class'Output (S'Access, Data);
         S.Dump;
      end Write_Memory_Stream;

      Message        : aliased Stream_Tools.Tests.BUG20220913.Eth.Messages.Command_Jak.Command_Jak_Message;

   begin
      Write_Memory_Stream (Message);
   end Test_Memory_Stream;

   --------------------
   -- Register_Tests --
   --------------------
   overriding procedure Register_Tests (T : in out Test_Case) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Crash_GNAT23'Access, "Crash_GNAT23");
      Register_Routine (T, Test_Memory_Stream'Access, "Test_Memory_Stream");
   end Register_Tests;

   Test_Name : constant String := GNAT.Source_Info.Enclosing_Entity;
   overriding function Name (T : Test_Case) return Message_String is
      pragma Unreferenced (T);
   begin
      return Format (Test_Name);
   end Name;

end Stream_Tools.Tests.BUG20220913;
