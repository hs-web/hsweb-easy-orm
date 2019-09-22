package org.hswebframework.ezorm.spring.mapping;

import org.hswebframework.ezorm.rdb.mapping.ReactiveRepository;
import org.hswebframework.ezorm.spring.executor.ReactiveTestApplication;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import static org.junit.Assert.*;


@SpringBootTest(classes = ReactiveTestApplication.class)
@RunWith(SpringJUnit4ClassRunner.class)
public class EasyormRepositoryScannerRegistrarTest {


    @Autowired
    ReactiveRepository<TestEntity,String> repository;


    @Test
    public void test(){
        Assert.assertNotNull(repository);
    }

}