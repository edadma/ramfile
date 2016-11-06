package xyz.hyperreal.ramfile


object TestMain extends App {
	val file = new RamFile( "test" )
	
	println( file.length )
	file.setLength( 200 )
	println( file.length )
	file.seek( 100 )
	file.writeInt( 0x12345678 )
	file.dump
}