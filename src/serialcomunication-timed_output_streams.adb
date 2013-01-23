with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with GNAT.Sockets;
with GNAT.Time_Stamp;

package body Serialcomunication.Timed_Output_Streams is
   use Ada.Directories;
   use Ada.Integer_Text_IO;
   use Ada.Float_Text_IO;
   use Ada.Real_Time;
   use Ada.Streams;
   use Ada.Text_IO;

   ------------
   -- Create --
   ------------

   not overriding procedure Create
     (Stream  : in out Timed_Output_Stream;
      Path    : String;
      Output_Base : Ada.Text_IO.Number_Base := 16;
      With_Header : Boolean := True;
      Append      : Boolean := False;
      Spacing     : Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span (0.1))
   is
   begin
      Stream.Cursor := Stream.Buffer'First;
      Stream.Spacing := Spacing;
      Stream.Output_Base := Output_Base;
      Stream.With_Header := With_Header;
      if Append then
         if Exists (Path) then
            Open (Stream.Target, Append_File, Path);
         else
            Create (Stream.Target, Out_File, Path);
         end if;
      else
         Create (Stream.Target, Out_File, Path);
      end if;
      if Stream.With_Header then
         Put_Line (Stream.Target, "-- -----------------------------------------------");
         Put_Line (Stream.Target, "--  Sampling started : " & GNAT.Time_Stamp.Current_Time);
         Put_Line (Stream.Target, "--    On host        : " & GNAT.Sockets.Host_Name);
         Put_Line (Stream.Target, "--    By user        : " & Ada.Environment_Variables.Value ("USER"));
         Put_Line (Stream.Target, "-- -----------------------------------------------");
         New_Line (Stream.Target);
      end if;
      Stream.Message_Time := Clock;
      Stream.Start_Time := Clock;
   end Create;

   -----------
   -- Close --
   -----------

   not overriding procedure Close
     (Stream  : in out Timed_Output_Stream)
   is
   begin
      Stream.Write;
      if Stream.With_Header then
         New_Line (Stream.Target);
         Put_Line (Stream.Target, "-- -----------------------------------------------");
         Put_Line (Stream.Target, "--  Sampling Ended    : " & GNAT.Time_Stamp.Current_Time);
         Put_Line (Stream.Target, "--  Sampling Duration :" & To_Duration (Clock - Stream.Start_Time)'Img);
         Put_Line (Stream.Target, "-- -----------------------------------------------");
      end if;
      Close (Stream.Target);
   end Close;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (Stream : in out Timed_Output_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is
   begin
      if Stream.Cursor > Stream.Buffer'First and then
        (Clock - Stream.Message_Time) > Stream.Spacing  then
         Stream.Write;
         Stream.Message_Time := Clock;
      end if;
      Stream.Buffer (Stream.Cursor .. Stream.Cursor + Item'Length - 1) := Item;
      Stream.Cursor := Stream.Cursor + Item'Length;
   end Write;
   not overriding procedure Write
     (Stream : in out Timed_Output_Stream) is
   begin
      Put (Stream.Target, Float (To_Duration (Clock - Stream.Message_Time)), Fore => 1, Aft => 3, Exp => 0);
      Put (Stream.Target, "; ");
      for I of Stream.Buffer (Stream.Buffer'First .. Stream.Cursor - 1) loop
         Put (Stream.Target, Integer (I), 4, Stream.Output_Base);
         Put (Stream.Target, " ");
      end loop;
      Stream.Cursor := Stream.Buffer'First;
      New_Line (Stream.Target);
   end Write;

end Serialcomunication.Timed_Output_Streams;
