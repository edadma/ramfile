package xyz.hyperreal.ramfile


object Main extends App {
	
	val file = new RamFile
	
	file.writeUTF( "testasdfas" )
	file.writeInt( 123654 )
	file.seek( 0 )
	println( file.readUTF )
	println( file.readInt )
}