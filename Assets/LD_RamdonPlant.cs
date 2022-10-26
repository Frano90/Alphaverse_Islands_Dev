using System.Collections.Generic;
using UnityEngine;
[ExecuteInEditMode]
public class LD_RamdonPlant : MonoBehaviour
{
    [SerializeField] Transform root;
    [SerializeField] GameObject[] plants;
    [SerializeField] List<Transform> clinds;
   
    void Awake()
    {
        //if(root.childCount >= 1)
        //{
        //    Transform[] clindsVirtual = root.GetComponentsInChildren<Transform>();
        //    foreach (Transform t in clindsVirtual)
        //    {
        //        clinds.Add(t);

        //    }
           
        //    foreach (Transform t in clinds)
        //    {
        //        if(t != root)
        //        {
        //            print("Deberiamos romper los hijos");
        //            DestroyImmediate(t.gameObject);
        //        }
           
        //    }
        //    clinds.Clear();
        //}
        SetPlant();
    }


  

    void SetPlant()
    {
        int d = Random.Range(0, plants.Length);
        Transform p = Instantiate(plants[d].transform, root);
        float y = Random.Range(0, 360); 
        float z = Random.Range(0, 10);
        Quaternion rota = Quaternion.Euler(0, y, z);
        //Vector3 rota = new Vector3(x, 0f, z);
        p.rotation = rota;
        this.enabled = false;
    }
}
