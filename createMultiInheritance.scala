import java.io._
import util.Random.nextInt

object createMultiInheritance{
  // Generate Tbox
  def creatOntology(depth: Int, max_branches: Int, max_inheritance: Int, writer: PrintWriter, list_nodes: List[Int], lastConcept: Int, list_multi: List[Int]) : (Int, List[Int]) = {
    /*
    if(depth == 0)
      return group_num + 1;
    var branches = min_branches + nextInt( max_branches - min_branches + 1);
    var max_group = group_num + 1;
    if(is_tree){
      //if we want to generate a structure in form of a tree
      for(x <- Range(0,branches)){

        writer.write("<http://www.Department" + depart + ".University" + univ + ".edu/ResearchGroup" + max_group + "> <" + property + "> <http://www.Department" + depart + ".University" + univ + ".edu/ResearchGroup" + group_num + "> .\n")
        max_group = creatOneChain(depth - 1, min_branches, max_branches, is_tree, max_group, property, writer, univ, depart);
      }
    }else{
      //if it is a chain with branches
      for(x <- Range(0,branches)){
        writer.write("<http://www.Department" + depart + ".University" + univ + ".edu/ResearchGroup" + (max_group + x) + "> <" + property + "> <http://www.Department" + depart + ".University" + univ + ".edu/ResearchGroup" + group_num + "> .\n")
      }
      max_group = creatOneChain(depth - 1, min_branches, max_branches, is_tree, max_group + branches - 1, property, writer, univ, depart);
    }
    return max_group;*/

    if(depth == 0)
      return (lastConcept, list_multi);
    else{
      var list_new_nodes: List[Int] = Nil;
      var list_new_multi: List[Int] = Nil;
      var branches = 0;
      var concept_num = lastConcept;
      for(i <- list_nodes){
        branches = nextInt(max_branches + 1);
        var num_inheritance = 0;

        // For each new concept
        for(x <- Range(1,branches + 1)){
          writer.write("<http://www.multiinhertance.com/concept" + (concept_num + x) + "> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://www.multiinhertance.com/concept" + i + "> .\n");
          // A concept has 30% opportunity of multiple inheritance
          if(nextInt(100) > 60){
            num_inheritance = Math.min(nextInt(max_inheritance) + 1, concept_num);

            var list_temp = List(i);
            var flag = true;
            for(y <- Range(1, num_inheritance) if flag){
              var super_node = nextInt(lastConcept + 1);
              while( list_temp.contains(super_node)){
                super_node = nextInt(lastConcept + 1);
              }
              writer.write("<http://www.multiinhertance.com/concept" + (concept_num + x) + "> <http://www.w3.org/2000/01/rdf-schema#subClassOf> <http://www.multiinhertance.com/concept" + super_node + "> .\n");
              list_temp = list_temp :+ super_node;
              if(list_temp.length > lastConcept)
                flag = false;
            }
            if(list_temp.length > 1)
              list_new_multi = list_new_multi :+ (concept_num + x);
          }
          //println("x = " + x);
          //println("node " + (concept_num + x));
          list_new_nodes = list_new_nodes :+ (concept_num + x);
        }


        concept_num += branches;
      }

      return creatOntology(depth - 1, max_branches, max_inheritance, writer, list_new_nodes, concept_num, list_multi ::: list_new_multi);
    }

  }

  // Generate Abox
  def createFacts(writer: PrintWriter, concept_number: Int): Int ={
    var num = 0;
    var node = 0;
    var list_existence: List[Int] = Nil;
    for(i <- Range(0, concept_number/2)){
      node = nextInt(concept_number);
      while(list_existence.contains(node)){
        node = nextInt(concept_number);
      }
      list_existence = list_existence :+ node;
      writer.write("<http://www.multiinhertance.com/instance" + num + "> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.multiinhertance.com/concept" + node + "> .\n");
      num += 1;
    }
    return num;
  }

  val usage = "Usage: createMultiInheritance [--max-depth num] [--max-branches num] [--path string] [--max-inheritance num]"


  def main(args: Array[String]) {
    var max_depth = 3;
    var max_branches = 1;
    var max_inheritance = 3;
    var reppath = "";


    if(args.length == 0) println(usage);
    val arglist = args.toList;


    def nextOption(list: List[String]) : Unit = {
      list match {
        case Nil =>
          return;
        case "--max-depth" :: value :: tail =>
          max_depth = value.toInt; nextOption(tail);
        case "--max-inheritance" :: value :: tail =>
          max_inheritance = value.toInt; nextOption(tail);
        case "--max-branches" :: value :: tail =>
          max_branches = value.toInt; nextOption(tail);
        case "--path" :: value :: tail =>
          reppath = value; nextOption(tail);
        case option :: tail =>
          println(option + " is a unknown option!!"); println(usage); System.exit(0);
      }
      return;
    }

    nextOption(arglist);



    val writer_ontology = new PrintWriter(new File(reppath + "TBOX" + "_depth" + max_depth + "_branches" + "_up_to" + max_branches + "max_inheritance_" + max_inheritance + ".nt"));
    val writer_fact = new PrintWriter(new File(reppath + "ABOX" + "_depth" + max_depth + "_branches" + "_up_to" + max_branches + "max_inheritance_" + max_inheritance + ".nt"));;

    val concepts_results = creatOntology(max_depth, max_branches, max_inheritance, writer_ontology, List(0), 0, Nil);
    val concepts_number = concepts_results._1 + 1;
    val list_multi_inheritance_nodes = concepts_results._2;

    println("Multi inheritance nodes: " + list_multi_inheritance_nodes);

    val max_instance_number = createFacts(writer_fact, concepts_number);

    println("File created")
    println("Generate a ontology which contains " + concepts_number + " concepts with a maximum inheritance of " + max_inheritance + ".")
    writer_ontology.close();
    writer_fact.close();
  }
}

