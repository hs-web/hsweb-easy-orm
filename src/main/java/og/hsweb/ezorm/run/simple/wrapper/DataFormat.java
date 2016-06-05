package og.hsweb.ezorm.run.simple.wrapper;

public interface DataFormat {
    Object format(Class javaType, Object value);
}
