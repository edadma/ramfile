package xyz.hyperreal.ramfile

import java.io.{IOException, Closeable, DataInput, DataOutput}
import java.nio.ByteBuffer


class RamFile extends Closeable {// with DataInput with DataOutput {
	val data = new ExpandableByteBuffer
	var closed = false

	private def eof = getFilePointer == length
	
	private def back =
		if (getFilePointer == 0)
			sys.error( "can't go back" )
		else
			seek( getFilePointer - 1 )
			
	def close {
		if (closed)
			throw new IOException( "closed" )
		else
			closed = true
	}
	
	def length: Long = data.size
	
	def getFilePointer: Long = data.buffer.position
	
	def read = {
		if (eof)
			-1
		else
			readByte&0xFF
	}
	
	def readBoolean =
		readByte match {
			case 1 => true
			case 0 => false
			case _ => sys.error( "invalid boolean value" )
		}

	def readByte = {
		data.getting( 1 )
		data.buffer.get
	}
	
	def readChar = {
		data.getting( 2 )
		data.buffer.getChar
	}
	
	def readDouble = {
		data.getting( 8 )
		data.buffer.getDouble
	}
	
	def readFloat = {
		data.getting( 4 )
		data.buffer.getFloat
	}
	
	def readFully( b: Array[Byte] ) {readFully( b, 0, b.length )}
	
	def readFully( b: Array[Byte], off: Int, len: Int ) {
		data.getting( off + len )
		data.buffer.get( b, off, len )
	}
	
	def readInt = {
		data.getting( 4 )
		data.buffer.getInt
	}
	
	def readLine = {
		""
	}
	
	def readShort = {
		data.getting( 2 )
		data.buffer.getShort
	}
	
	def readUnsignedShort = {
		data.getting( 2 )
		data.buffer.getShort&0xFFFF
	}
	
	def readUTF = {
		val len = readUnsignedShort
		val b = new Array[Byte]( len )
		
		readFully( b )
		new String( io.Codec.fromUTF8(b) )
	}
	
	def seek( p: Long ) {
		assert( p <= data.size, "file pointer must be less than or equal to file size" )
		data.buffer.position( p.asInstanceOf[Int] )
	}
	
	def setLength( l: Long ) = data.size = l.asInstanceOf[Int]
	
	def write( b: Array[Byte] ) {
		data.putting( b.length )
		data.buffer.put( b )
	}
	
	def write( b: Int ) = writeByte( b )
		
	def writeBoolean( a: Boolean ) = writeByte( if (a) 1 else 0 )

	def writeByte( b: Int ) {
		data.putting( 1 )
		data.buffer.put( b.asInstanceOf[Byte] )
	}
	
	def writeInt( v: Int ) {
		data.putting( 4 )
		data.buffer.putInt( v )
	}
	
	def writeShort( v: Int ) {
		data.putting( 2 )
		data.buffer.putShort( v.asInstanceOf[Short] )
	}
	
	def writeUTF( s: String ) {
		val b = io.Codec.toUTF8( s )
		
		writeShort( b.length )
		write( b )
	}
}