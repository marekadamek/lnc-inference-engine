package nclogic.java;

import nclogic.java.model.HistoryGraph;
import nclogic.java.model.expr.Expr;
import nclogic.java.parser.Parser;
import org.junit.Test;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;

public class Test2 {

    @Test
    public void test() throws URISyntaxException, IOException {
        Parser parser = new Parser();

        List<String> fileLines = Files.readAllLines(Paths.get(ClassLoader.getSystemResource("farmerDilemma.txt").toURI()));

        String file = fileLines.stream()
                //.map(l -> l.replaceAll( "(?m)\s+$", ""))
                .filter(l -> !l.startsWith("#"))
                .collect(Collectors.joining(""));

        Expr formula = parser.parse(file);

        HistoryGraph graph = new HistoryGraph(formula);

        Expr from = parser.parse("!f & !w & !s & !c").simplify();
        Expr to = parser.parse("f & w & s & c").simplify();

        List<Expr> path = graph.findPath(from, to);
        assertEquals(8, path.size());
        assertEquals(from, path.get(0));
        assertEquals(to, path.get(path.size() - 1));
    }
}
