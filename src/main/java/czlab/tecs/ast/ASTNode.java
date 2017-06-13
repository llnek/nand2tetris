package czlab.tecs.ast;

import java.io.Writer;

/**
 * @author Kenneth Leung
 */
public class ASTNode extends SimpleNode {

  /**
   */
  public ASTNode(int i) {
    super(i);
  }

  /**
   */
  public ASTNode(ASTGentor p, int i) {
    super(p,i);
  }

  public void dumpEDN(Writer w) throws Exception {
    w.write("[\n");
    dumpEDN(0,w);
    w.write("\n]\n");
  }

  /**
   */
  private void dumpEDN(int level, Writer w) throws Exception {
    String pad=mkStr(level *2);
    Object v= jjtGetValue();
    String k= toString();
    String stag= ":" + k;
    int slen=stag.length();
    boolean hasC= children != null &&
                  children.length > 0;

    if (hasC) {
      w.write(pad + stag + " [\n");
      for (int i = 0; i < children.length; ++i) {
        ASTNode n = (ASTNode)children[i];
        if (n != null) {
          n.dumpEDN(level+1, w);
        }
      }
      w.write(pad + mkStr(slen) + " ]\n");
    } else {
      w.write(pad + stag + " " +
              (v==null
               ? "nil"
               : "\"" +v + "\"") + "\n");
    }
  }

  public void dumpXML(Writer w) throws Exception {
    dumpXML(0,w);
  }

  /**
   */
  private void dumpXML(int level, Writer w) throws Exception {
    String k= toString();
    String stag= "<" + k +">";
    String etag= "<" + k +"/>";
    String pad=mkStr(level * 2);
    Object v= jjtGetValue();
    boolean hasC= children != null &&
                  children.length > 0;
    if (hasC) {
      w.write(pad + stag + "\n");
      for (int i = 0; i < children.length; ++i) {
        ASTNode n = (ASTNode)children[i];
        if (n != null) {
          n.dumpXML(level+1, w);
        }
      }
      w.write(pad + etag + "\n");
    } else {
      w.write(pad + stag +
              (v==null ? "" : v) + etag + "\n");
    }
  }

  public int getId() {
    return id;
  }

  private String mkStr(int len) {
    StringBuilder b=new StringBuilder();
    for (int i=0; i < len; ++i) {
      b.append(" ");
    }
    return b.toString();
  }

}

/* JavaCC - OriginalChecksum=7aa0be25bd21cbd061295b9d3295ddae (do not edit this line) */
