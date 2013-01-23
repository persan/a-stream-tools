with Ada.Streams;
with Ada.Real_Time;
with Ada.Text_IO;
package Serialcomunication.Timed_Output_Streams is
   type Timed_Output_Stream (Size : Ada.Streams.Stream_Element_Offset)
     is new Ada.Streams.Root_Stream_Type with private;

   not overriding procedure Create
     (Stream  : in out Timed_Output_Stream;
      Path    : String;
      Output_Base : Ada.Text_IO.Number_Base := 16;
      With_Header : Boolean := True;
      Append      : Boolean := False;
      Spacing : Ada.Real_Time.Time_Span := Ada.Real_Time.To_Time_Span (0.1));

   not overriding procedure Close
     (Stream  : in out Timed_Output_Stream);

   overriding procedure Read
     (Stream : in out Timed_Output_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is null;

   overriding procedure Write
     (Stream : in out Timed_Output_Stream;
      Item   : Ada.Streams.Stream_Element_Array);

private
   type Timed_Output_Stream (Size : Ada.Streams.Stream_Element_Offset)
     is  new Ada.Streams.Root_Stream_Type with record
      Buffer       : Ada.Streams.Stream_Element_Array (1 .. Size);
      Cursor       : Ada.Streams.Stream_Element_Offset;
      Last         : Ada.Streams.Stream_Element_Offset;
      Target       : Ada.Text_IO.File_Type;
      Spacing      : Ada.Real_Time.Time_Span;
      Message_Time : Ada.Real_Time.Time;
      Start_Time   : Ada.Real_Time.Time;
      Output_Base  : Ada.Text_IO.Number_Base;
      With_Header  : Boolean;
   end record;
   not overriding procedure Write
     (Stream : in out Timed_Output_Stream);

end Serialcomunication.Timed_Output_Streams;
