using System.Collections;
using System.Collections.Generic;
using Unity.VisualScripting.Dependencies.Sqlite;
using UnityEditor.PackageManager;
using UnityEngine;
[ExecuteInEditMode]
public class LD_PrefabPlacer : MonoBehaviour
{
  
  
        public enum state
        {
           pause,
           delete,
            set
        }
       
        public state stateMethod;
    [Header("Prefabs")]
    [SerializeField] GameObject[] prefabs;
    [SerializeField] Transform[] positions;
  
    [SerializeField] float minScalex;
    [SerializeField] float minScalez;
    [SerializeField] float minScaley;
    [SerializeField] float maxScalex;
    [SerializeField] float maxScalez;
    [SerializeField] float maxScaley;
    [SerializeField] bool setPos;
    


    // Update is called once per frame

    
    void Update()
        {
        //if(setPos == false)
        //{
        //    positions = GetComponentsInChildren<Transform>();
        //    setPos = true;
        //}
        SetPrefab();

        }


        void SetPrefab()
        {
            switch (stateMethod)
            {
            case state.pause:
                break;
            case state.delete:
                foreach (Transform t in positions)
                {

                    t.DetachChildren();
                }
                stateMethod = state.pause;
                break;
            case state.set:
                foreach (Transform t in positions)
                {
                
                    int i = Random.Range(0, prefabs.Length);
                    var v = Instantiate(prefabs[i], t);
                    float Vx = Random.Range(minScalex, maxScalex); 
                    float Vy = Random.Range(minScaley, maxScaley); 
                    float Vz = Random.Range(minScalez, maxScalez);
                    v.transform.localScale.Set(Vx, Vy, Vz);
                }
                stateMethod = state.pause;
                break;
            default:
                break;
            }
        }
       
}

