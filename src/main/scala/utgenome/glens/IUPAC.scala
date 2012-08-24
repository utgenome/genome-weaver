package utgenome.glens

//--------------------------------------
//
// IUPAC.scala
// Since: 2012/03/16 11:25
//
//--------------------------------------

/**
 * IUPAC code of DNA alleles
 * @author leo
 */
object IUPAC {
  object * extends IUPAC("*", 0x00)
  object A extends IUPAC("A", 0x01)
  object C extends IUPAC("C", 0x02)
  object G extends IUPAC("G", 0x04)
  object T extends IUPAC("T", 0x08)
  object M extends IUPAC("A/C", 0x03)
  object R extends IUPAC("A/G", 0x05)
  object W extends IUPAC("A/T", 0x09)
  object S extends IUPAC("C/G", 0x06)
  object Y extends IUPAC("C/T", 0x0A)
  object K extends IUPAC("G/T", 0x0C)
  object V extends IUPAC("A/C/G", 0x07)
  object H extends IUPAC("A/C/T", 0x0B)
  object D extends IUPAC("A/G/T", 0x0D)
  object B extends IUPAC("C/G/T", 0x0E)
  object N extends IUPAC("A/C/G/T", 0x0F)

  val values = Array(*, A, C, G, T, M, R, W, S, Y, K, V, H, D, B, N)

  private[glens] val bitFlagToIUPACTable = {
    val table = new Array[IUPAC](values.length)
    for(each <- values) {
      table(each.bitFlag & 0x0F) = each
    }
    table
  }

  private[glens] lazy val symbolTable = {
    values.map(each => each.letter -> each).toMap
  }

  private[glens] val complementTable = Array[IUPAC](*, T, G, K, C, Y, S, B, A, W,
    R, D, M, H, V, N)


  def genotypeToIUPAC(genoType:String) : IUPAC = {
    val flag = genoType.foldLeft(0){(flag, ch) =>
      val code = DNA.to2bitCode(ch)
      flag | (1 << code)
    }
    bitFlagToIUPACTable(flag & 0x0F)
  }

  def toIUPAC(symbol:String) : Option[IUPAC] = symbolTable.get(symbol)

}

sealed abstract class IUPAC(val variation:String, val bitFlag:Int) extends GenomeLetter {

  def complement : IUPAC = IUPAC.complementTable(bitFlag)

  /**
   * String representation of genotype (e.g., ACG for V)
   */
  lazy val genotype : String = {
    val genoType = new StringBuilder(4)
    def loop(index:Int) {
      val flag = 1 << index
      if(index < 4) {
        if((bitFlag & flag) != 0)
          genoType += DNA.decode(index.toByte).toChar
        loop(index+1)
      }
    }
    loop(0)
    genoType.result
  }


}