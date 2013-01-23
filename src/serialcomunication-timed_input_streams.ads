with Ada.Streams;
private with Ada.Text_IO;
private with Ada.Real_Time;
package Serialcomunication.Timed_Input_Streams is
   type Timed_Input_Stream (Size : Ada.Streams.Stream_Element_Offset)
     is new Ada.Streams.Root_Stream_Type with private;

   not overriding procedure Open
     (Stream  : in out Timed_Input_Stream;
      Path    : String);

   not overriding procedure Close
     (Stream  : in out Timed_Input_Stream);

   overriding procedure Read
     (Stream : in out Timed_Input_Stream;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Timed_Input_Stream;
      Item   : Ada.Streams.Stream_Element_Array)
   is null;

private
   type Timed_Input_Stream (Size : Ada.Streams.Stream_Element_Offset)
     is new Ada.Streams.Root_Stream_Type with record
      Buffer : Ada.Streams.Stream_Element_Array (1 .. Size);
      Cursor : Ada.Streams.Stream_Element_Offset;
      Last   : Ada.Streams.Stream_Element_Offset;
      Source : Ada.Text_IO.File_Type;
      Next_Time : Ada.Real_Time.Time;
   end record;
end Serialcomunication.Timed_Input_Streams;
