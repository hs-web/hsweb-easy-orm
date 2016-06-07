package org.hsweb.ezorm.param;

/**
 * Created by zhouhao on 16-5-9.
 */
public interface TermType {
    /**
     * ==
     */
    String eq = "eq";
    /**
     * !=
     */
    String not = "not";
    /**
     * like
     */
    String like = "like";
    String notlike = "notlike";
    /**
     * >
     */
    String gt = "gt";
    /**
     * <
     */
    String lt = "lt";
    /**
     * >=
     */
    String gtoreq = "gtoreq";
    /**
     * <=
     */
    String ltoreq = "ltoreq";
    /**
     * in
     */
    String in = "in";
    /**
     * notin
     */
    String notin = "notin";
    /**
     * =''
     */
    String empty = "empty";
    /**
     * !=''
     */
    String notempty = "notempty";
    /**
     * is null
     */
    String isnull = "isnull";
    /**
     * not null
     */
    String notnull = "notnull";
    /**
     * between
     */
    String btw = "btw";
    /**
     * not between
     */
    String notbtw = "notbtw";
    String func = "func";
}
