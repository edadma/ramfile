package xyz.hyperreal.ramfile

import org.scalatest._
import prop.PropertyChecks


class Tests extends FreeSpec with PropertyChecks with Matchers {
	
	"lines" in {
		val file = new RamFile( "lines" )
		
		file.length shouldBe 0
		file.writeBytes( "hello\r\n" )
		file.writeBytes( "world\r" )
		file.writeBytes( "this is a test\n" )
		file.writeInt( 123654 )
		file.seek( 0 )
		file.readLine shouldBe "hello"
		file.readLine shouldBe "world"
		file.readLine shouldBe "this is a test"
		file.readInt shouldBe 123654
		file.length shouldBe 32
	}
	
	"UTF-8" in {
		val file = new RamFile( "UTF-8" )
		
		file.length shouldBe 0
		file.writeUTF( "hello" )
		file.writeUTF( "this is a test" )
		file.writeInt( 123654 )
		file.seek( 0 )
		file.readUTF shouldBe "hello"
		file.readUTF shouldBe "this is a test"
		file.readInt shouldBe 123654
		file.length shouldBe 27
	}
	
}