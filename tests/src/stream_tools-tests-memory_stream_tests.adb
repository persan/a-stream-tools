pragma Ada_2022;
with Stream_Tools.Memory_Streams;
with Ada.Streams;
with AUnit.Assertions;
with Ada.IO_Exceptions;
package body Stream_Tools.Tests.Memory_Stream_Tests is
   use AUnit.Assertions;
   use Stream_Tools.Memory_Streams;
   use Ada.Streams;
   function Image (Item : Stream_Element_Array) return String;

   function Image (Item : Stream_Element_Array) return String is
      type Nible is range 0 .. 15 with
         Size => 4;
      type Nible_Array is array (1 .. Item'Length * 2) of Nible with
         Pack => True;
      Src : Nible_Array with
         Address => Item'Address;
      Map    : constant array (Nible) of Character := "0123456789ABCDEF";
      Cursor : Natural                             := 1;
   begin
      return Ret : String (1 .. Item'Length * 2) do
         for I of Src loop
            Ret (Cursor) := Map (I);
            Cursor       := Cursor + 1;
         end loop;
      end return;
   end Image;
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
      Register_Routine (T, Test_Size'Access, "Test_Size");
      Register_Routine (T, Test_As_String'Access, "Test_As_String");
      Register_Routine (T, Test_As_Source'Access, "Test_As_Source");
      Register_Routine (T, Test_Indexining'Access, "Test_Indexining");
      Register_Routine (T, Test_Image'Access, "Test_Image");

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
      S      : aliased Stream_Tools.Memory_Streams.Memory_Stream;
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
      S      : aliased Stream_Tools.Memory_Streams.Memory_Stream;
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
      S          : aliased Stream_Tools.Memory_Streams.Memory_Stream;
      Buffer     : aliased Ada.Streams.Stream_Element_Array (1 .. 4);
      Out_Buffer : aliased constant Ada.Streams.Stream_Element_Array (1 .. 4) := [1, 2, 3, 4];
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
      S          : aliased Stream_Tools.Memory_Streams.Memory_Stream;
      Buffer     : aliased Ada.Streams.Stream_Element_Array (1 .. 4) := [0, 1, 2, 3];
      Out_Buffer : aliased Ada.Streams.Stream_Element_Array (1 .. 3);
      Dummy_Last : Ada.Streams.Stream_Element_Offset;
   begin
      S.Set_Address (Buffer'Address);
      S.Set_Length (Buffer'Length);
      S.Read (Out_Buffer, Dummy_Last);
      begin
         S.Read (Out_Buffer, Dummy_Last);
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
      S          : aliased Memory_Stream;
      Buffer     : aliased Stream_Element_Array (1 .. 4)             := [0, 1, 2, 3];
      Out_Buffer : aliased Ada.Streams.Stream_Element_Array (1 .. 3) := [others => 16#CA#];
      Dummy_Last       : Ada.Streams.Stream_Element_Offset;
   begin
      S.Set_Address (Buffer'Address);
      S.Set_Length (Buffer'Length);

      S.Read (Out_Buffer, Dummy_Last);
      Assert (Out_Buffer = [0, 1, 2], "Wrong Data");
      S.Set_End_Of_File_Stretegy (To => Return_Remaing_Data_Or_Raise);
      Out_Buffer := [others => 16#FF#];
      S.Read (Out_Buffer, Dummy_Last);
      Assert
        (Out_Buffer = [3, 16#FF#, 16#FF#],
         "Wrong Data got [" & Image (Out_Buffer) & "] expected [" & Image ([3, 16#FF#, 16#FF#]) & "]");
      Assert (Dummy_Last = Out_Buffer'First, "Wrong amount read, expexted last to be :  1 Got : " & Dummy_Last'Img);
      begin
         S.Read (Out_Buffer, Dummy_Last);
         Assert (True, "Could Read from empty buffer");
      exception
         when Ada.IO_Exceptions.End_Error =>
            null;
         when others =>
            Assert (True, "Wrong exception raised");
      end;
   end Test_Read_Element_Array_3;

   procedure Test_Dump (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S        : aliased Dynamic_Memory_Stream (16, As_Needed);
      S2       : aliased Dynamic_Memory_Stream (16, As_Needed);
      Expected : constant String := "30 31 32 33 34 35 36 37 38 39";
   begin
      String'Write (S'Access, "0123456789");
      S.Dump (S2'Access);
      declare
         Actual : constant String := S2.As_Standard_String;
      begin
         Assert (Expected = Actual, "Content missmatch, Expected:'" & Expected & "', got :'" & Actual & "'.");
      end;
   end Test_Dump;

   procedure Test_As_String (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S           : aliased Dynamic_Memory_Stream (16, As_Needed);
      Expected_16 : constant String := "1234567890ABCDEF";
      Expected_4  : constant String := "abcd";
      Expected_4b : constant String := "abcd567890ABCDEF";

   begin
      S.Reset;
      String'Write (S'Access, Expected_16);
      Assert
        (S.As_Standard_String = Expected_16,
         "Content missmatch, Expected:'" & Expected_16 & "', got :'" & S.As_Standard_String & "'.");
      S.Reset;
      String'Write (S'Access, Expected_4);
      Assert
        (S.As_Standard_String = Expected_4,
         "Content missmatch, Expected:'" & Expected_4 & "', got :'" & S.As_Standard_String & "'.");
      Assert
        (S.As_Standard_String (True) = Expected_4b,
         "Content missmatch, Expected:'" & Expected_4b & "', got :'" & S.As_Standard_String (True) & "'.");
   end Test_As_String;

   procedure Test_As_Source (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S        : aliased Dynamic_Memory_Stream (64, As_Needed);
      Tgt      : aliased Dynamic_Memory_Stream (64, As_Needed);
      Expected : constant String := "(16#20#, 16#22#, 16#23#, 16#FF#);";
   begin
      S.Reset;
      Tgt.Reset;
      Stream_Element'Write (S'Access, 16#20#);
      Stream_Element'Write (S'Access, 16#22#);
      Stream_Element'Write (S'Access, 16#23#);
      Stream_Element'Write (S'Access, 16#FF#);
      S.As_Source (Tgt'Access);
      declare
         Actual : constant String := Tgt.As_Standard_String;
      begin
         Assert (Actual = Expected, "Content missmatch, Expected:'" & Expected & "', got :'" & Actual & "'.");
      end;
   end Test_As_Source;

   procedure Test_Size (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S1       : aliased Memory_Stream;
      S2       : aliased Memory_Stream;
      Buffer1  : aliased Stream_Element_Array (1 .. 10) := [others => 16#CA#];
      Buffer2  : aliased Stream_Element_Array (1 .. 10) := [others => 16#FF#];
      Expected : constant Stream_Element_Array := [1 => 16#44#, 2 => 16#33#, 3 => 16#22#, 4 => 16#11#, 5 .. 10 => 16#FF#];
   begin
      S1.Set_Address (Buffer1'Address);
      S1.Set_Length (Buffer1'Length);
      S1.Reset;
      Integer'Write (S1'Access, 16#11_22_33_44#);

      S2.Set_Address (Buffer2'Address);
      S2.Set_Length (Buffer2'Length);
      S2.Reset;
      Memory_Stream'Write (S2'Access, S1);
      Assert (Buffer2 = Expected, "Got:" & Image (Buffer2) & " expected:" & Image (Expected));
   end Test_Size;
   procedure Test_Indexining (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S1       : aliased Memory_Stream;
      Buffer  : aliased Stream_Element_Array (1 .. 10) := [others => 16#00#];
   begin
      S1.Set_Address (Buffer'Address);
      S1.Set_Length (Buffer'Length);
      Buffer (1) := 2;
      Buffer (10) := 10;
      Assert (Buffer (1) = S1 (1), "Got:" & Image (S1 (1)) & " expected:" & Image (Buffer (1)));
      Assert (Buffer (10) = S1 (10), "Got:" & Image (S1 (10)) & " expected:" & Image (Buffer (10)));
      begin
         S1 (11) := S1 (1);
      exception
         when Constraint_Error => null;
      end;
      begin
         S1 (1) := S1 (11);
      exception
         when Constraint_Error => null;
      end;
   end Test_Indexining;
   procedure Test_Image (T : in out Test_Cases.Test_Case'Class) is
      pragma Unreferenced (T);
      S1       : aliased Memory_Stream;
      Buffer   : aliased Stream_Element_Array (1 .. 60) := [others => 16#00#];
      Excpected_32 : constant String :=
                       "[16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#," & ASCII.LF &
                       " 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#, 16#20#]";
   begin
      S1.Set_Address (Buffer'Address);
      S1.Set_Length (Buffer'Length);
      Assert (S1'Image = "[]", "Got:" & S1'Image & ", expected: []");
      Character'Write (S1'Access, ' ');
      Assert (S1'Image = "[16#20#]", "Got:" & S1'Image & ", expected: [16#20#]");
      Character'Write (S1'Access, ' ');
      Assert (S1'Image = "[16#20#, 16#20#]", "Got:" & S1'Image & ", expected: [16#20#, 16#20#]");
      String'Write (S1'Access, "                              ");
      Assert (S1'Image = Excpected_32, "Got:" & S1'Image & ", expected: " & Excpected_32);
   end Test_Image;

end Stream_Tools.Tests.Memory_Stream_Tests;
