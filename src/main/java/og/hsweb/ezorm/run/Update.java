package og.hsweb.ezorm.run;

import og.hsweb.ezorm.param.UpdateParam;

import java.sql.SQLException;

/**
 * Created by zhouhao on 16-6-4.
 */
public interface Update<T> {
    Update<T> set(T data);

    Update<T> set(String property, Object value);

    Update<T> where(String condition, Object value);

    Update<T> includes(String... fields);
    
    Update<T> excludes(String... fields);

    Update<T> setParam(UpdateParam<T> param);

    int exec() throws SQLException;
}
