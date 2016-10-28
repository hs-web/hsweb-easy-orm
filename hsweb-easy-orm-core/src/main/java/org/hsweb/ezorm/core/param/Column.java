package org.hsweb.ezorm.core.param;

/**
 * @author zhouhao
 * @since 1.1
 */
public class Column {
    private String name;

    private String type;

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
