//import nclogic.LncInferenceEngine;
//import nclogic.model.DnfConverter;
//import nclogic.model.HistoryGraph;
//import nclogic.model.Types;
//import nclogic.parser.FormulaReader;
//import nclogic.parser.Parser;
//import scala.collection.Iterator;
//import scala.collection.immutable.List;
//import scala.collection.immutable.Set;
//import scala.io.Codec;
//import scala.io.Source;
//import scala.util.Try;
//
//public class Main {
//
//    public static void main(String[] args) {
//        String file = "/farmerDilemma.txt";
//        String line = FormulaReader.read(Source.fromURL(Main.class.getResource(file), Codec.UTF8()));
//        //val line = "(a | b) & (c | d)"
//        Try<Types.Expr> formula = Parser.parse(line);
//        HistoryGraph graph = LncInferenceEngine.getHistoryGraph(formula.get());
//
//
//        Set<Types.Expr> from = DnfConverter.convert(Parser.parse("!bc & !bf & !bs & !bw & !rc & !rf & !rs & !rw").get()).head();
//        Set<Types.Expr> to = DnfConverter.convert(Parser.parse("!bc & !bf & !bs & !bw & rc & rf & rs & rw").get()).head();
//
//
//        List<Set<Types.Expr>> successors = graph.getSuccessors(from).toList();
//        for (int i = 0; i < successors.size(); ++i) {
//            List<Types.Expr> suc = successors.apply(i).toList();
//            for (int j = 0; j < successors.size(); ++j) {
//                Types.Expr e = suc.apply(j);
//                String s = e.toString();
//            }
//        }
//        List<Set<Types.Expr>> path = graph.findPath(from, to);
//        Iterator<Set<Types.Expr>> it = path.iterator();
//        while (it.hasNext()) {
//            System.out.println(it.next());
//        }
//    }
//}
