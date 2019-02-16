
// el jar esta en MAVEN, se puede exportar

import _root_.hex.genmodel.GenModel
import _root_.hex.genmodel.easy.{EasyPredictModelWrapper, RowData}
import _root_.hex.genmodel.easy.prediction
import _root_.hex.genmodel.MojoModel
import _root_.hex.genmodel.easy.RowData
import org.apache.spark.sql.types.{StructType, StructField, StringType, IntegerType, DoubleType}


// cargar mi modelo 
val modelPath = "epa_glm.zip"
// Cargar datos de test para predecir
val dataPath = "epa_test.csv"

// Import data
val epa = spark.read.option("header", "true").
csv(dataPath)
// Import MOJO model
val mojo = MojoModel.load(modelPath)

val easyModel = new EasyPredictModelWrapper( 
                new EasyPredictModelWrapper.Config().
                setModel(mojo).
                setConvertUnknownCategoricalLevelsToNa(true).
                setConvertInvalidNumbersToNa(true))

// -------------

// Convertir  todas las columnas a rowdata
// -------------

val header = epa.columns

// Predict
val epa_score = epa.map {
  x =>
    val r = new RowData
    header.indices.foreach(idx => r.put(header(idx), x.getAs[String](idx) ))
    val score = easyModel.predictBinomial(r).classProbabilities
   (x.getAs[String](0),x.getAs[String](3), score(1))
  }.toDF("label","prov","predict")
  
  
epa_score.show(false)


//val epa_score2 = epa_score.withColumn("label_double", col("label").cast(DoubleType)) 


epa_score.filter(col("label")===1).show

epa_score.groupBy("label").agg(avg("predict")).show
epa_score.groupBy("prov").agg(avg("label"),avg("predict")).show(52)

