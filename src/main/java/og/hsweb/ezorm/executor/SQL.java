package og.hsweb.ezorm.executor;

import og.hsweb.ezorm.meta.TableMetaData;

import java.util.List;

/**
 * sql模板对象接口,提供需要执行sql的模板以及参数等信息
 * Created by 浩 on 2015-11-06 0006.
 */
public interface SQL {

    TableMetaData getTableMetaData();

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
