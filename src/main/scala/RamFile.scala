package xyz.hyperreal.ramfile

import collection.immutable.TreeMap

import java.io.{IOException, Closeable, DataInput, DataOutput}
import java.nio.ByteBuffer


object RamFile {
	private var files = new TreeMap[String, ExpandableByteBuffer]
	
	def list = files.keys.toList
	
	def exists( filename: String ) = RamFile.list exists (f => f == filename)
	
	def delete( filename: String ) = files -= filename
}

class RamFile( filename: String ) extends Closeable with DataInput with DataOutput {
	var data =
		RamFile.files.get( filename ) match {
			case Some( buf ) => buf
			case None =>
				val buf = new ExpandableByteBuffer
				
				RamFile.files += filename -> buf
				buf
		}

	private def eof = getFilePointer == length
	
	private def back =
		if (getFilePointer == 0)
			sys.error( "can't go back" )
		else
			seek( getFilePointer - 1 )
			
	def close = data = null
	
	def length = data.size.toLong
	
	def getFilePointer = data.buffer.position.toLong
	
	def read =
		if (eof)
			-1
		else
			readByte&0xFF
	
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
		data.getting( len )
		data.buffer.get( b, off, len )
	}
	
	def readInt = {
		data.getting( 4 )
		data.buffer.getInt
	}
	
	def readLine = {
		val buf = new StringBuilder
		
		def readnext: String = {
			read match {
				case -1 =>
					if (buf isEmpty)
						null
					else
						buf.toString
				case c =>
					c.toChar match {
						case '\n' =>
							buf.toString
						case '\r' =>
							if (!eof)
								if (read.toChar != '\n')
									back
								
								buf.toString
						case ch =>
							buf += ch
							readnext
					}
			}
		}
		
		readnext
	}
	
	def readLong = {
		data.getting( 8 )
		data.buffer.getLong
	}
	
	def readShort = {
		data.getting( 2 )
		data.buffer.getShort
	}
	
	def readUnsignedByte = readByte&0xFF
	
	def readUnsignedShort = readShort&0xFFFF
	
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
	
	def skipBytes( n: Int ) =
		if (n > 0) {
			val skipped = (getFilePointer + n) min (length - getFilePointer)
			
			seek( skipped )
			skipped.toInt
		} else
			0
	
	def write( b: Array[Byte] ) = write( b, 0, b.length )
	
	def write( b: Array[Byte], off: Int, len: Int ) {
		data.putting( len )
		data.buffer.put( b, off, len )
	}
	
	def write( b: Int ) = writeByte( b )
		
	def writeBoolean( a: Boolean ) = writeByte( if (a) 1 else 0 )

	def writeByte( b: Int ) {
		data.putting( 1 )
		data.buffer.put( b.asInstanceOf[Byte] )
	}
	
	def writeBytes( s: String ) = write( s.toCharArray map (c => c.toByte) )
	
	def writeChar( v: Char ) {
		data.putting( 2 )
		data.buffer.putChar( v )
	}
	
	def writeChar( v: Int ) = writeChar( v.toChar )
	
	def writeChars( s: String ) =
		for (c <- s)
			writeChar( c )
	
	def writeDouble( v: Double ) {
		data.putting( 8 )
		data.buffer.putDouble( v )
	}
	
	def writeFloat( v: Float ) {
		data.putting( 4 )
		data.buffer.putFloat( v )
	}
	
	def writeInt( v: Int ) {
		data.putting( 4 )
		data.buffer.putInt( v )
	}
	
	def writeLong( v: Long ) {
		data.putting( 8 )
		data.buffer.putLong( v )
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
	
	def dump {
		val cur = getFilePointer
		val width = 16
		
		seek( 0 )
		
		def printByte( b: Int ) = print( "%02x ".format(b&0xFF).toUpperCase )
		
		def printChar( c: Int ) = print( if (' ' <= c && c <= '~') c.asInstanceOf[Char] else '.' )
		
		for (line <- 0L until length by width) {
			printf( s"%10x  ", line )
			
			val mark = getFilePointer
			
			for (i <- line until ((line + width) min length)) {
				if (i%16 == 8)
					print( ' ' )
					
				printByte( readByte )
			}
			
			val bytes = (getFilePointer - mark).asInstanceOf[Int]
			
			print( " "*((width - bytes)*3 + 1 + (if (bytes < 9) 1 else 0)) )
			
			seek( mark )
			
			for (i <- line until ((line + width) min length))
				printChar( readByte.asInstanceOf[Int] )
				
			println
		}
		
		seek( cur )
	}
}