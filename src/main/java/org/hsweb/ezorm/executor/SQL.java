package org.hsweb.ezorm.executor;

import java.util.List;

public interface SQL {
    /**
     * 获取sql语句模板
     *
     * @return sql语句模板
     */
    String getSql();

    /**
     * 获取预编译参数
     *
     * @return
     */
    Object getParams();

    /**
     * 获取关联查询的sql
     *
     * @return
     */
    List<BindSQL> getBinds();

    int size();
}
