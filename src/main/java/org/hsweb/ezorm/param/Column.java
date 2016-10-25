package org.hsweb.ezorm.param;

/**
 * @author zhouhao
 * @since 1.1
 */
public class Column {
    private String name;

    private String type;

    private String as;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getAs() {
        if (as == null) as = name;
        return as;
    }

    public Column as(String as) {
        this.as = as;
        return this;
    }

    public Column type(String type) {
        this.type = type;
        return this;
    }

    public static Column build(String name) {
        Column column = new Column();
        column.setName(name);
        return column;
    }
}
