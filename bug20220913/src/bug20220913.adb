with Stream_Tools.Memory_Streams;
procedure BUG20220913 is
--  type PI_Memory_Stream is new Stream_Tools.Memory_Streams.Dynamic_Memory_Stream (1024, Stream_Tools.Memory_Streams.Multiply_By_Two) with null record;
--  S : aliased PI_Memory_Stream;

   S : aliased Stream_Tools.Memory_Streams.Dynamic_Memory_Stream (1024, Stream_Tools.Memory_Streams.Multiply_By_Two);
begin

   Integer'Write (S'Access, 2);
end BUG20220913;
