with Stream_Tools.Memory_Streams;
with Ada.Streams;
with AUnit.Assertions;
with Ada.IO_Exceptions;
package body Stream_Tools.Tests.Memory_Stream_Tests is
   use AUnit.Assertions;
   use Stream_Tools.Memory_Streams;
   use Ada.Streams;
   --------------------
   -- Register_Tests --
   --------------------

   overriding procedure Register_Tests (T : in out Memory_Stream_Test) is
      use AUnit.Test_Cases.Registration;
   begin
      Register_Routine (T, Test_Simple'Access, "Test_Simple");
      Register_Routine (T, Test_Read_Past_Eof_Raise'Access, "Test_Read_Past_Eof_Raise");
      Register_Routine (T, Test_Read_Element_Array_1'Access, "Test_Read_Element_Array_1");
      Register_Routine (T, Test_Read_Element_Array_2'Access, "Test_Read_Element_Array_2");
      Register_Routine (T, Test_Read_Element_Array_3'Access, "Test_Read_Element_Array_3");
   end Register_Tests;

   ----------
   -- Name --
   ----------

   overriding function Name (T : Memory_Stream_Test) return Message_String is
      pragma Unreferenced (T);
   begin
      return Format ("Stream_Tools.Tests.Memory_Stream_Tests");
   end Name;

   -----------------
   -- Test_Simple --
   -----------------

   procedure Test_Simple (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S : aliased Stream_Tools.Memory_Streams.Memory_Stream;
      Buffer : aliased Ada.Streams.Stream_Element_Array (1 .. 4);
      Temp   : Integer := 0;
   begin
      S.Set_Address (Buffer'Address);
      S.Set_Length (Buffer'Length);
      Integer'Write (S'Access, 16#11_22_33_44#);
      S.Reset;
      Integer'Read (S'Access, Temp);
      Assert (Temp = 16#11_22_33_44#, "Corupt data");
   end Test_Simple;

   ------------------------------
   -- Test_Read_Past_Eof_Raise --
   ------------------------------

   procedure Test_Read_Past_Eof_Raise (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S : aliased Stream_Tools.Memory_Streams.Memory_Stream;
      Buffer : aliased Ada.Streams.Stream_Element_Array (1 .. 4);
      Temp   : Integer := 0;
   begin
      S.Set_Address (Buffer'Address);
      S.Set_Length (Buffer'Length);
      Integer'Write (S'Access, 16#11_22_33_44#);
      S.Reset;
      Integer'Read (S'Access, Temp);
      begin
         Integer'Read (S'Access, Temp);
         Assert (True, "Could read past endof buffer");
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
         when others =>
            Assert (True, "Wring exception raised");
      end;
   end Test_Read_Past_Eof_Raise;

   procedure Test_Read_Element_Array_1 (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S : aliased Stream_Tools.Memory_Streams.Memory_Stream;
      Buffer : aliased Ada.Streams.Stream_Element_Array (1 .. 4);
      Out_Buffer : aliased constant Ada.Streams.Stream_Element_Array (1 .. 4) := (1, 2, 3, 4);
   begin
      S.Set_Address (Buffer'Address);
      S.Set_Length (Buffer'Length);
      S.Write (Out_Buffer);
      begin
         S.Write (Out_Buffer);
         Assert (True, "Could Write past endof buffer");
      exception
         when Constraint_Error =>
            null;
         when others =>
            Assert (True, "Wrong exception raised");
      end;
   end Test_Read_Element_Array_1;

   procedure Test_Read_Element_Array_2 (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S : aliased Stream_Tools.Memory_Streams.Memory_Stream;
      Buffer : aliased Ada.Streams.Stream_Element_Array (1 .. 4) := (0, 1, 2, 3);
      Out_Buffer : aliased Ada.Streams.Stream_Element_Array (1 .. 3);
      Last       : Ada.Streams.Stream_Element_Offset;
   begin
      S.Set_Address (Buffer'Address);
      S.Set_Length (Buffer'Length);
      S.Read (Out_Buffer, Last);
      begin
         S.Read (Out_Buffer, Last);
         Assert (True, "Could Read past endof buffer");
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
         when others =>
            Assert (True, "Wrong exception raised");
      end;

   end Test_Read_Element_Array_2;

   procedure Test_Read_Element_Array_3 (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S : aliased Memory_Stream;
      Buffer : aliased Stream_Element_Array (1 .. 4) := (0, 1, 2, 3);
      Out_Buffer : aliased Ada.Streams.Stream_Element_Array (1 .. 3);
      Last       : Ada.Streams.Stream_Element_Offset;
   begin
      S.Set_Address (Buffer'Address);
      S.Set_Length (Buffer'Length);

      S.Read (Out_Buffer, Last);
      Assert (Out_Buffer = (0, 1, 2), "Wrong Data");
      S.Set_End_Of_File_Stretegy (To => Return_Remaing_Data_Or_Raise);
      Out_Buffer := (others =>  16#FF#);
      S.Read (Out_Buffer, Last);
      Assert (Out_Buffer = (3, 16#FF#, 16#FF#), "Wrong Data got [" &
                Image (Out_Buffer) & "] expected ["  & Image ((3, 16#FF#, 16#FF#)) & "]");
      Assert (Last = Out_Buffer'First, "To mutch data read");
      begin
         S.Read (Out_Buffer, Last);
         Assert (True, "Could Read from empty buffer");
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
         when others =>
            Assert (True, "Wrong exception raised");
      end;
   end Test_Read_Element_Array_3;
end Stream_Tools.Tests.Memory_Stream_Tests;
