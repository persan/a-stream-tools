with Ada.Strings.Fixed;
with GNAT.String_Split;

package body Stream_Tools.Timed_Input_Streams is

   use Ada.Text_IO;
   use Ada.Streams;
   use Ada.Real_Time;
   use Ada.Strings;
   use Ada.Strings.Fixed;
   ----------
   -- Open --
   ----------

   not overriding procedure Open
     (Stream  : in out Timed_Input_Stream;
      Path    : String)
   is
   begin
      Open (Stream.Source, In_File, Path);
      Stream.Cursor := Stream.Buffer'First;
      Stream.Last := Stream.Cursor - 1;
      Stream.Next_Time := Clock;
   end Open;

   -----------
   -- Close --
   -----------

   not overriding procedure Close
     (Stream  : in out Timed_Input_Stream)
   is
   begin
      Close (Stream.Source);
   end Close;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (Stream : in out Timed_Input_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
      procedure Parse (S  : String);
      procedure Parse (S  : String) is
         S1 : GNAT.String_Split.Slice_Set;
         S2 : GNAT.String_Split.Slice_Set;
         Data_Slice : GNAT.String_Split.Slice_Number := 1;
         use GNAT.String_Split;
      begin
         Create (S1, S, ";");
         if Slice_Count (S1) = 2 then
            Stream.Next_Time := Stream.Next_Time + To_Time_Span (Duration'Value (Slice (S1, 1)));
            Data_Slice := 2;
         end if;
         Create (S2, Slice (S1, Data_Slice), " ", Multiple);
         for I in 1 .. Slice_Count (S2) loop
            declare
               Val : constant String := Slice (S2, I);
            begin
               if Val'Length > 0 then
                  Stream.Buffer (Stream.Cursor) := Stream_Element'Value (Val);
                  Stream.Cursor := Stream.Cursor + 1;
               end if;
            end;
         end loop;
         Stream.Last := Stream.Cursor - 1;
         Stream.Cursor := Stream.Buffer'First;
      end Parse;
   begin
      if Stream.Cursor > Stream.Last then
         Stream.Cursor := Stream.Buffer'First;
         Read_Loop : while not End_Of_File (Stream.Source) loop
            declare
               Buffer : constant String := Trim (Get_Line (Stream.Source), Both);
            begin
               if Buffer'Length < 2 then
                  null;
               elsif Buffer (Buffer'First) = '#' then
                  null;
               elsif Buffer (Buffer'First .. Buffer'First + 1) = "--" then
                  null;
               else
                  Parse (Buffer);
                  delay until Stream.Next_Time;
                  exit Read_Loop;
               end if;
            end;
         end loop Read_Loop;
         delay until Stream.Next_Time;
      end if;
      if Stream.Cursor + Item'Length - 1 > Stream.Last then
         raise Constraint_Error with "oversalple";
      end if;
      Item := Stream.Buffer (Stream.Cursor .. Stream.Cursor + Item'Length - 1);
      Stream.Cursor := Stream.Cursor + Item'Length;
      Last := Item'Last;
   end Read;

end Stream_Tools.Timed_Input_Streams;
