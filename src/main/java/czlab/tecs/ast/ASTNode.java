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

  /**
   */
  public void dump(String prefix, Writer w) throws Exception {
    String k= toString();
    String stag= "<" + k +">";
    String etag= "<" + k +"/>";
    Object v= jjtGetValue();
    boolean hasC= children != null &&
                  children.length > 0;
    if (hasC) {
      w.write(prefix + stag + "\n");
      for (int i = 0; i < children.length; ++i) {
        ASTNode n = (ASTNode)children[i];
        if (n != null) {
          n.dump(prefix + " ", w);
        }
      }
      w.write(prefix + etag + "\n");
    } else {
      w.write(prefix + stag +
              (v==null ? "" : v) + etag + "\n");
    }
  }

  public int getId() {
    return id;
  }
}

/* JavaCC - OriginalChecksum=7aa0be25bd21cbd061295b9d3295ddae (do not edit this line) */
