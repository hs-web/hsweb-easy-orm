package og.hsweb.ezorm.param;

/**
 * Created by zhouhao on 16-5-9.
 */
public enum TermType {
    /**
     * ==
     */
    eq,
    /**
     * !=
     */
    not,
    /**
     * like
     */
    like,
    notlike,
    /**
     * >
     */
    gt,
    /**
     * <
     */
    lt,
    /**
     * >=
     */
    gtoreq,
    /**
     * <=
     */
    ltoreq,
    /**
     * in
     */
    in,
    /**
     * notin
     */
    notin,
    /**
     * =''
     */
    empty,
    /**
     * !=''
     */
    notempty,
    /**
     * is null
     */
    isnull,
    /**
     * not null
     */
    notnull,
    /**
     * between
     */
    btw,
    /**
     * not between
     */
    notbtw,

    func;

    public static TermType fromString(String str) {
        if (str == null || !str.contains("$")) {
            return eq;
        } else {
            try {
                return valueOf(str.split("[\\$]")[1].toLowerCase());
            } catch (Exception e) {
                return eq;
            }
        }
    }
}
