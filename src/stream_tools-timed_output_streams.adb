-------------------------------------------------------------------------------
--                                                                           --
--  Copyright 2015 Per Sandberg <per.s.sandberg@bahnhof.se>                  --
--                                                                           --
--  Permission is hereby granted, free of charge, to any person obtaining a  --
--  copy of this software and associated documentation files                 --
--  (the "Software"), to deal in the Software without restriction, including --
--  without limitation the rights to use, copy, modify, merge, publish,      --
--  distribute, sublicense, and / or sell copies of the Software, and to     --
--  permit persons to whom the Software is furnished to do so, subject to    --
--  the following conditions :                                               --
--                                                                           --
--  The above copyright notice and this permission notice shall be included  --
--  in all copies or substantial portions of the Software.                   --
--                                                                           --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
--  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
--  MERCHANTABILITY,                                                         --
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL  --
--  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR     --
--  OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,    --
--  ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR    --
--  OTHER DEALINGS IN THE SOFTWARE.                                          --
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Environment_Variables;
with Ada.Float_Text_IO;
with Ada.Integer_Text_IO;
with GNAT.Sockets;
with GNAT.Time_Stamp;

package body Stream_Tools.Timed_Output_Streams is
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
     (Stream      : in out Timed_Output_Stream;
      Path        : String;
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
         Stream.Put_Header;
      end if;
      Stream.Message_Time := Clock;
      Stream.Start_Time := Clock;
   end Create;

   not overriding procedure Set_Output_Base
     (Stream  : in out Timed_Output_Stream; To : Ada.Text_IO.Number_Base) is
   begin
      Stream.Output_Base := To;
   end Set_Output_Base;
   not overriding procedure Put_Line
     (Stream  : in out Timed_Output_Stream;
      Item    : String) is
   begin
      Put_Line (Stream.Target, Item);
   end Put_Line;

   not overriding procedure New_Line
     (Stream  : in out Timed_Output_Stream;
      Spacing : Ada.Text_IO.Positive_Count := 1) is
   begin
      New_Line (Stream.Target, Spacing);
   end New_Line;

   -----------
   -- Close --
   -----------

   not overriding procedure Close
     (Stream  : in out Timed_Output_Stream)
   is
   begin
      Stream.Write;
      if Stream.With_Header then
         Stream.Put_Footer;
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
      if (Stream.Cursor > Stream.Buffer'First and then
            (Clock - Stream.Message_Time) > Stream.Spacing) or
        (Stream.Cursor + Item'Length > Stream.Buffer'Last)
      then
         Stream.Flush;
      end if;
      Stream.Buffer (Stream.Cursor .. Stream.Cursor + Item'Length - 1) := Item;
      Stream.Cursor := Stream.Cursor + Item'Length;
   end Write;

   not overriding procedure Flush
     (Stream : in out Timed_Output_Stream) is
   begin
      Stream.Write;
      Stream.Tick;
   end Flush;

   not overriding procedure Tick
     (Stream : in out Timed_Output_Stream) is
   begin
      Stream.Message_Time := Clock;
   end Tick;

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

   not overriding procedure Put_Header
     (Stream  : in out Timed_Output_Stream) is
   begin
      Stream.Put_Line ("-- -----------------------------------------------");
      Stream.Put_Line ("--  Sampling started : " & GNAT.Time_Stamp.Current_Time);
      Stream.Put_Line ("--    On host        : " & GNAT.Sockets.Host_Name);
      Stream.Put_Line ("--    By user        : " & Ada.Environment_Variables.Value ("USER"));
      Stream.Put_Line ("-- -----------------------------------------------");
   end Put_Header;
   not overriding procedure Put_Footer
     (Stream  : in out Timed_Output_Stream) is
   begin         Stream.New_Line;
      Stream.Put_Line ("-- -----------------------------------------------");
      Stream.Put_Line ("--  Sampling Ended    : " & GNAT.Time_Stamp.Current_Time);
      Stream.Put_Line ("--  Sampling Duration :" & To_Duration (Clock - Stream.Start_Time)'Img);
      Stream.Put_Line ("-- -----------------------------------------------");

   end Put_Footer;

end Stream_Tools.Timed_Output_Streams;
